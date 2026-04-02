import std/[math, random, times, sequtils, strformat, typetraits]

# ============================================
# SIMD Configuration and Types
# Enable SIMD optimizations
# nim c -d:avx -d:release --opt:speed backprop_simd.nim
# Run
# ./backprop_simd
# ============================================

const
  SimdWidth* = 4  # AVX/SSE width for float64 (4 doubles = 256 bits)
  UseSimd* = defined(avx) or defined(sse) or defined(avx2)

when UseSimd:
  type
    Float64x4* = simd[float64, SimdWidth]

  proc simdLoad*(aptr: ptr float64): Float64x4 {.inline.} =
    result = simd[float64, SimdWidth](aptr)

  proc simdStore*(aptr: ptr float64, val: Float64x4) {.inline.} =
    simdStore(aptr, val)

# ============================================
# Matrix Structure with SIMD Alignment
# ============================================

type
  Matrix* = object
    data*: seq[float64]
    rows*, cols*: int
    stride*: int  # Row stride for SIMD alignment

  LayerType* = enum
    ltLinear, ltSigmoid, ltReLU, ltTanh

  Layer* = ref object
    weights*: Matrix
    biases*: Matrix
    layerType*: LayerType
    # Caches for backpropagation
    z*: Matrix  # Pre-activation values
    a*: Matrix  # Post-activation values
    input*: Matrix  # Input cache for gradient computation

  Network* = ref object
    layers*: seq[Layer]
    learningRate*: float64

# ============================================
# SIMD-Optimized Matrix Operations
# ============================================

proc newMatrix*(rows, cols: int, zero = true): Matrix =
  ## Create a new matrix with SIMD-aligned stride
  let stride = ((cols + SimdWidth - 1) div SimdWidth) * SimdWidth
  result.rows = rows
  result.cols = cols
  result.stride = stride
  result.data = newSeq[float64](rows * stride)
  if zero:
    for i in 0..<result.data.len:
      result.data[i] = 0.0

proc index*(m: Matrix, row, col: int): int {.inline.} =
  ## Calculate linear index (supports SIMD stride)
  row * m.stride + col

proc `[]`*(m: Matrix, row, col: int): float64 {.inline.} =
  m.data[m.index(row, col)]

proc `[]=`*(m: var Matrix, row, col: int, val: float64) {.inline.} =
  m.data[m.index(row, col)] = val

proc randomMatrix*(rows, cols: int, scale: float64 = 0.1): Matrix =
  ## Initialize with Xavier/Glorot initialization
  result = newMatrix(rows, cols)
  for i in 0..<rows:
    for j in 0..<cols:
      result[i, j] = (rand(1.0) * 2 - 1) * scale

proc printMatrix*(m: Matrix, name: string = "") =
  if name.len > 0:
    stdout.write(name & ":\n")
  for i in 0..<m.rows:
    stdout.write("[")
    for j in 0..<m.cols:
      stdout.write(&" {m[i,j]:8.4f}")
    stdout.write(" ]\n")
  stdout.write("\n")

# ============================================
# SIMD-Optimized Matrix Multiplication
# ============================================

proc matMul*(a, b: Matrix): Matrix =
  ## SIMD-optimized matrix multiplication: C = A * B
  ## Mathematical principle: C_{ij} = Σ_k A_{ik} * B_{kj}

  if a.cols != b.rows:
    raise newException(ValueError, "Matrix dimensions mismatch")

  result = newMatrix(a.rows, b.cols)

  when UseSimd:
    # SIMD-optimized version with blocking for cache efficiency
    const BlockSize = 32

    for ii in countup(0, a.rows-1, BlockSize):
      let iMax = min(ii + BlockSize, a.rows)
      for jj in countup(0, b.cols-1, BlockSize):
        let jMax = min(jj + BlockSize, b.cols)

        for i in ii..<iMax:
          for j in jj..<jMax:
            var sum: float64 = 0.0

            # Process in SIMD chunks
            var k = 0
            while k + SimdWidth <= a.cols:
              # Load vectors from A and B
              var aVec: Float64x4
              var bVec: Float64x4

              for idx in 0..<SimdWidth:
                aVec[idx] = a[i, k + idx]
                bVec[idx] = b[k + idx, j]

              # SIMD multiply and horizontal add
              let mulVec = aVec * bVec
              sum += mulVec[0] + mulVec[1] + mulVec[2] + mulVec[3]
              k += SimdWidth

            # Handle remaining elements
            while k < a.cols:
              sum += a[i, k] * b[k, j]
              k += 1

            result[i, j] = sum
  else:
    # Fallback to standard multiplication
    for i in 0..<a.rows:
      for k in 0..<a.cols:
        let aik = a[i, k]
        if aik != 0.0:
          for j in 0..<b.cols:
            result[i,j] = result[i,j] + aik * b[k, j]

proc matAdd*(a, b: Matrix): Matrix =
  ## Element-wise matrix addition with SIMD

  if a.rows != b.rows or a.cols != b.cols:
    raise newException(ValueError, "Matrix dimensions mismatch")

  result = newMatrix(a.rows, a.cols)
  let totalSize = a.rows * a.stride

  when UseSimd:
    var i = 0
    while i + SimdWidth <= totalSize:
      let aVec = simdLoad(addr a.data[i])
      let bVec = simdLoad(addr b.data[i])
      let sumVec = aVec + bVec
      simdStore(addr result.data[i], sumVec)
      i += SimdWidth

    # Handle remainder
    while i < totalSize:
      result.data[i] = a.data[i] + b.data[i]
      i += 1
  else:
    for i in 0..<totalSize:
      result.data[i] = a.data[i] + b.data[i]

proc matSub*(a, b: Matrix): Matrix =
  ## Element-wise matrix subtraction with SIMD

  if a.rows != b.rows or a.cols != b.cols:
    raise newException(ValueError, "Matrix dimensions mismatch")

  result = newMatrix(a.rows, a.cols)
  let totalSize = a.rows * a.stride

  when UseSimd:
    var i = 0
    while i + SimdWidth <= totalSize:
      let aVec = simdLoad(addr a.data[i])
      let bVec = simdLoad(addr b.data[i])
      let diffVec = aVec - bVec
      simdStore(addr result.data[i], diffVec)
      i += SimdWidth

    while i < totalSize:
      result.data[i] = a.data[i] - b.data[i]
      i += 1
  else:
    for i in 0..<totalSize:
      result.data[i] = a.data[i] - b.data[i]

proc scalarMul*(m: Matrix, scalar: float64): Matrix =
  ## Multiply matrix by scalar with SIMD

  result = newMatrix(m.rows, m.cols)
  let totalSize = m.rows * m.stride

  when UseSimd:
    let scalarVec = Float64x4(splat: scalar)
    var i = 0
    while i + SimdWidth <= totalSize:
      let mVec = simdLoad(addr m.data[i])
      let prodVec = mVec * scalarVec
      simdStore(addr result.data[i], prodVec)
      i += SimdWidth

    while i < totalSize:
      result.data[i] = m.data[i] * scalar
      i += 1
  else:
    for i in 0..<totalSize:
      result.data[i] = m.data[i] * scalar

proc transpose*(m: Matrix): Matrix =
  ## Matrix transpose

  result = newMatrix(m.cols, m.rows)
  for i in 0..<m.rows:
    for j in 0..<m.cols:
      result[j, i] = m[i, j]

# ============================================
# Activation Functions with SIMD
# ============================================

proc sigmoid*(z: Matrix): Matrix =
  ## Sigmoid activation: σ(z) = 1/(1 + e^(-z))
  ## Mathematical property: σ'(z) = σ(z)(1-σ(z))

  result = newMatrix(z.rows, z.cols)
  let totalSize = z.rows * z.stride

  when UseSimd:
    var i = 0
    while i + SimdWidth <= totalSize:
      let zVec = simdLoad(addr z.data[i])
      var resultVec: Float64x4
      for idx in 0..<SimdWidth:
        resultVec[idx] = 1.0 / (1.0 + exp(-zVec[idx]))
      simdStore(addr result.data[i], resultVec)
      i += SimdWidth

    while i < totalSize:
      result.data[i] = 1.0 / (1.0 + exp(-z.data[i]))
      i += 1
  else:
    for i in 0..<totalSize:
      result.data[i] = 1.0 / (1.0 + exp(-z.data[i]))

proc sigmoidDerivative*(a: Matrix): Matrix =
  ## Derivative of sigmoid: σ'(z) = a * (1 - a)
  ## where a = σ(z)

  result = newMatrix(a.rows, a.cols)
  let totalSize = a.rows * a.stride

  when UseSimd:
    var i = 0
    while i + SimdWidth <= totalSize:
      let aVec = simdLoad(addr a.data[i])
      let oneVec = Float64x4(splat: 1.0)
      let resultVec = aVec * (oneVec - aVec)
      simdStore(addr result.data[i], resultVec)
      i += SimdWidth

    while i < totalSize:
      let av = a.data[i]
      result.data[i] = av * (1.0 - av)
      i += 1
  else:
    for i in 0..<totalSize:
      let av = a.data[i]
      result.data[i] = av * (1.0 - av)

proc relu*(z: Matrix): Matrix =
  ## ReLU activation: f(z) = max(0, z)
  ## Mathematical property: f'(z) = 1 if z > 0 else 0

  result = newMatrix(z.rows, z.cols)
  let totalSize = z.rows * z.stride

  when UseSimd:
    var i = 0
    while i + SimdWidth <= totalSize:
      let zVec = simdLoad(addr z.data[i])
      var resultVec: Float64x4
      for idx in 0..<SimdWidth:
        resultVec[idx] = max(0.0, zVec[idx])
        simdStore(addr result.data[i], resultVec)
      i += SimdWidth

    while i < totalSize:
      result.data[i] = max(0.0, z.data[i])
      i += 1
  else:
    for i in 0..<totalSize:
      result.data[i] = max(0.0, z.data[i])

proc reluDerivative*(z: Matrix): Matrix =
  ## Derivative of ReLU: f'(z) = 1 if z > 0 else 0

  result = newMatrix(z.rows, z.cols)
  let totalSize = z.rows * z.stride

  when UseSimd:
    var i = 0
    while i + SimdWidth <= totalSize:
      let zVec = simdLoad(addr z.data[i])
      var resultVec: Float64x4
      for idx in 0..<SimdWidth:
        resultVec[idx] = if zVec[idx] > 0.0: 1.0 else: 0.0
        simdStore(addr result.data[i], resultVec)
      i += SimdWidth

    while i < totalSize:
      result.data[i] = if z.data[i] > 0.0: 1.0 else: 0.0
      i += 1
  else:
    for i in 0..<totalSize:
      result.data[i] = if z.data[i] > 0.0: 1.0 else: 0.0

proc tanhActivation*(z: Matrix): Matrix =
  ## Tanh activation: tanh(z) = (e^z - e^{-z})/(e^z + e^{-z})
  ## Mathematical property: tanh'(z) = 1 - tanh^2(z)

  result = newMatrix(z.rows, z.cols)
  let totalSize = z.rows * z.stride

  for i in 0..<totalSize:
    result.data[i] = math.tanh(z.data[i])

# ============================================
# LU Decomposition for Matrix Inversion
# ============================================

proc luDecomposition*(a: Matrix): tuple[L, U: Matrix, pivot: seq[int]] =
  ## LU decomposition with partial pivoting
  ## Mathematical principle: A = P * L * U
  ## where L is lower triangular, U is upper triangular, P is permutation matrix

  let n = a.rows
  if n != a.cols:
    raise newException(ValueError, "Matrix must be square")

  var L = newMatrix(n, n)
  var U = newMatrix(n, n)
  var pivot = newSeq[int](n)

  # Copy A to U
  for i in 0..<n:
    pivot[i] = i
    for j in 0..<n:
      U[i, j] = a[i, j]

  # Gaussian elimination
  for k in 0..<n-1:
    # Find pivot
    var maxRow = k
    var maxVal = abs(U[k, k])
    for i in k+1..<n:
      let val = abs(U[i, k])
      if val > maxVal:
        maxVal = val
        maxRow = i

    if maxVal < 1e-12:
      raise newException(ValueError, "Matrix is singular")

    # Swap rows
    if maxRow != k:
      swap(pivot[k], pivot[maxRow])
      for j in 0..<n:
        let tmp = U[k, j]
        U[k, j] = U[maxRow, j]
        U[maxRow, j] = tmp
      if k > 0:
        for i in 0..<k:
          let tmp = L[k, i]
          L[k, i] = L[maxRow, i]
          L[maxRow, i] = tmp

    # Compute multipliers and update
    for i in k+1..<n:
      L[i, k] = U[i, k] / U[k, k]
      for j in k+1..<n:
        U[i, j] = U[i, j] - L[i, k] * U[k, j]
      U[i, k] = 0.0

  # Set diagonal of L to 1
  for i in 0..<n:
    L[i, i] = 1.0

  return (L, U, pivot)

proc inverseMatrix*(a: Matrix): Matrix =
  ## Compute matrix inverse using LU decomposition
  ## Mathematical principle: A * A^(-1) = I

  let n = a.rows
  if n != a.cols:
    raise newException(ValueError, "Matrix must be square")

  var (L, U, pivot) = luDecomposition(a)
  defer:
    L.data.setLen(0)
    U.data.setLen(0)

  result = newMatrix(n, n)

  # Solve A * X = I for each column of X
  for col in 0..<n:
    # Forward substitution: L * y = P * e_col
    var y = newSeq[float64](n)
    for i in 0..<n:
      var sum = if pivot[i] == col: 1.0 else: 0.0
      for j in 0..<i:
        sum -= L[i, j] * y[j]
      y[i] = sum

    # Backward substitution: U * x = y
    for i in countdown(n-1, 0):
      var sum = y[i]
      for j in i+1..<n:
        sum -= U[i, j] * result[j, col]
      result[i, col] = sum / U[i, i]

# ============================================
# Layer Implementation with Backpropagation
# ============================================

proc forward*(layer: Layer, input: Matrix): Matrix =
  ## Forward pass: a = f(W * x + b)
  ## Mathematical principle: Layer transformation with activation

  # Cache input for backward pass
  if layer.input.rows != input.rows or layer.input.cols != input.cols:
    layer.input = newMatrix(input.rows, input.cols)
  for i in 0..<input.rows * input.stride:
    layer.input.data[i] = input.data[i]

  # Compute z = W * x + b
  var wz = matMul(layer.weights, input)
  layer.z = matAdd(wz, layer.biases)
  wz.data.setLen(0)

  # Apply activation function
  case layer.layerType
  of ltLinear:
    layer.a = layer.z
  of ltSigmoid:
    layer.a = sigmoid(layer.z)
  of ltReLU:
    layer.a = relu(layer.z)
  of ltTanh:
    layer.a = tanhActivation(layer.z)

  return layer.a

proc backward*(layer: Layer, gradOutput: Matrix): tuple[gradW, gradB, gradInput: Matrix] =
  ## Backward pass: Compute gradients using chain rule
  ## Mathematical principle:
  ##   δ = ∂L/∂z = ∂L/∂a * ∂a/∂z
  ##   ∂L/∂W = δ * x^T
  ##   ∂L/∂b = δ
  ##   ∂L/∂x = W^T * δ

  # Step 1: Compute gradient w.r.t. pre-activation (δ = ∂L/∂z)
  var gradZ: Matrix
  case layer.layerType
  of ltLinear:
    gradZ = gradOutput
  of ltSigmoid:
    var sigGrad = sigmoidDerivative(layer.a)
    gradZ = matMul(gradOutput, sigGrad)
    sigGrad.data.setLen(0)
  of ltReLU:
    var relGrad = reluDerivative(layer.z)
    gradZ = matMul(gradOutput, relGrad)
    relGrad.data.setLen(0)
  of ltTanh:
    # tanh'(z) = 1 - tanh^2(z) = 1 - a^2
    var tanhGrad = newMatrix(layer.a.rows, layer.a.cols)
    for i in 0..<layer.a.rows * layer.a.stride:
      let a = layer.a.data[i]
      tanhGrad.data[i] = 1.0 - a * a
    gradZ = matMul(gradOutput, tanhGrad)
    tanhGrad.data.setLen(0)

  # Step 2: Compute gradients for weights and biases
  var inputT = transpose(layer.input)
  let gradW = matMul(gradZ, inputT)
  inputT.data.setLen(0)

  # Gradient for biases is just gradZ (sum over batch dimension)
  var gradB = newMatrix(gradZ.rows, gradZ.cols)
  for i in 0..<gradZ.rows * gradZ.stride:
    gradB.data[i] = gradZ.data[i]

  # Step 3: Compute gradient for input to propagate to previous layer
  var weightsT = transpose(layer.weights)
  let gradInput = matMul(weightsT, gradZ)
  weightsT.data.setLen(0)

  # Clean up
  if layer.layerType != ltLinear:
    gradZ.data.setLen(0)

  return (gradW, gradB, gradInput)

proc updateParams*(layer: Layer, gradW, gradB: Matrix, learningRate: float64) =
  ## Update layer parameters using gradient descent
  ## Mathematical principle: W_new = W_old - η * ∂L/∂W

  let scaledGradW = scalarMul(gradW, learningRate)
  let scaledGradB = scalarMul(gradB, learningRate)

  let newWeights = matSub(layer.weights, scaledGradW)
  let newBiases = matSub(layer.biases, scaledGradB)

  # Replace old parameters
  layer.weights.data = newWeights.data
  layer.weights.rows = newWeights.rows
  layer.weights.cols = newWeights.cols
  layer.weights.stride = newWeights.stride

  layer.biases.data = newBiases.data
  layer.biases.rows = newBiases.rows
  layer.biases.cols = newBiases.cols
  layer.biases.stride = newBiases.stride

# ============================================
# Complete Neural Network
# ============================================

proc newLayer*(inputSize, outputSize: int, layerType: LayerType): Layer =
  ## Create a new layer with Xavier initialization
  let scale = sqrt(2.0 / float64(inputSize + outputSize))
  result = Layer(
    weights: randomMatrix(outputSize, inputSize, scale),
    biases: randomMatrix(outputSize, 1, scale),
    layerType: layerType
  )

proc forward*(network: Network, input: Matrix): seq[Matrix] =
  ## Forward pass through entire network
  result = @[input]
  for layer in network.layers:
    let output = layer.forward(result[^1])
    result.add(output)

proc backward*(network: Network, input, target: Matrix): seq[tuple[gradW, gradB: Matrix]] =
  ## Backward pass computing gradients for all layers
  # Forward pass to get all activations
  var activations = network.forward(input)
  defer:
    for i in 1..<activations.len:
      activations[i].data.setLen(0)

  # Compute output error (derivative of MSE loss)
  # For MSE: L = 1/2 Σ(y - ŷ)^2, ∂L/∂ŷ = (ŷ - y)
  let outputError = matSub(activations[^1], target)

  # Backward pass
  var gradOutput = outputError
  result = newSeq[tuple[gradW, gradB: Matrix]](network.layers.len)

  for i in countdown(network.layers.len-1, 0):
    let (gradW, gradB, gradInput) = network.layers[i].backward(gradOutput)
    result[i] = (gradW, gradB)
    gradOutput = gradInput

proc train*(network: Network, input, target: Matrix, epochs: int) =
  ## Train the network using backpropagation and gradient descent

  for epoch in 0..<epochs:
    # Compute gradients
    let gradients = network.backward(input, target)

    # Update parameters
    for i in 0..<network.layers.len:
      var (gradW, gradB) = gradients[i]
      network.layers[i].updateParams(gradW, gradB, network.learningRate)

      # Clean up gradient matrices
      gradW.data.setLen(0)
      gradB.data.setLen(0)

    # Calculate and print loss
    let activations = network.forward(input)
    var loss = 0.0
    let output = activations[^1]
    for i in 0..<output.rows:
      let diff = output[i, 0] - target[i, 0]
      loss += 0.5 * diff * diff

    if epoch mod 100 == 0:
      echo &"Epoch {epoch:4d}"

# ============================================
# Mathematical Explanation and Demonstration
# ============================================

proc explainBackpropagation() =
  echo "\n" & "=".repeat(70)
  echo "MATHEMATICAL PRINCIPLES OF BACKPROPAGATION"
  echo "=".repeat(70)

  echo """
1. CHAIN RULE FUNDAMENTALS:
-------------------------
Backpropagation applies the chain rule from calculus:

∂L/∂W⁽ˡ⁾ = ∂L/∂a⁽ˡ⁾ · ∂a⁽ˡ⁾/∂z⁽ˡ⁾ · ∂z⁽ˡ⁾/∂W⁽ˡ⁾

where:
- L is the loss function
- W⁽ˡ⁾ are weights at layer l
- a⁽ˡ⁾ = f(z⁽ˡ⁾) is the activation
- z⁽ˡ⁾ = W⁽ˡ⁾·a⁽ˡ⁻¹⁾ + b⁽ˡ⁾ is the pre-activation

2. FORWARD PASS:
-------------
Information flows forward through the network:

Layer 1: z¹ = W¹·x + b¹,  a¹ = f(z¹)
Layer 2: z² = W²·a¹ + b²,  a² = f(z²)
...
Output:  ŷ = aᴸ

Loss:    L = ½(y - ŷ)²  (Mean Squared Error)

3. BACKWARD PASS (GRADIENT COMPUTATION):
--------------------------------------
Start from output and propagate errors backward:

δᴸ = ∂L/∂zᴸ = (ŷ - y) ⊙ f'(zᴸ)  (output error)
δˡ = ∂L/∂zˡ = (Wˡ⁺¹)ᵀ·δˡ⁺¹ ⊙ f'(zˡ)  (hidden layer error)

Gradients:
∂L/∂Wˡ = δˡ·(aˡ⁻¹)ᵀ
∂L/∂bˡ = δˡ
∂L/∂aˡ⁻¹ = (Wˡ)ᵀ·δˡ

4. ACTIVATION FUNCTIONS AND THEIR DERIVATIVES:
-------------------------------------------
Sigmoid:   σ(z) = 1/(1+e⁻ᶻ),    σ'(z) = σ(z)(1-σ(z))
ReLU:      f(z) = max(0,z),     f'(z) = 1 if z>0 else 0
Tanh:      tanh(z),             tanh'(z) = 1 - tanh²(z)

5. GRADIENT DESCENT UPDATE:
------------------------
W_new = W_old - η·∂L/∂W
b_new = b_old - η·∂L/∂b

where η is the learning rate.
"""

proc demonstrateMatrixInversion() =
  echo "\n" & "=".repeat(70)
  echo "MATRIX INVERSION DEMONSTRATION (LU Decomposition)"
  echo "=".repeat(70)

  echo """
LU Decomposition Principle:
A = P·L·U

where:
- P is a permutation matrix (partial pivoting)
- L is lower triangular with unit diagonal
- U is upper triangular

Then A⁻¹ = U⁻¹·L⁻¹·Pᵀ
"""

  # Create a test matrix
  var a = newMatrix(3, 3)
  a[0,0] = 2; a[0,1] = 1; a[0,2] = 1
  a[1,0] = 1; a[1,1] = 3; a[1,2] = 2
  a[2,0] = 1; a[2,1] = 2; a[2,2] = 2

  echo "Original matrix A:"
  a.printMatrix()

  # Compute inverse
  let inv = inverseMatrix(a)
  echo "Inverse matrix A⁻¹:"
  inv.printMatrix()

  # Verify: A * A⁻¹ should be identity
  let identity = matMul(a, inv)
  echo "Verification: A * A⁻¹ (should be identity):"
  identity.printMatrix()

proc demonstrateBackpropagation() =
  echo "\n" & "=".repeat(70)
  echo "BACKPROPAGATION DEMONSTRATION: XOR Problem"
  echo "=".repeat(70)

  echo """
XOR Problem: Neural network learning XOR logical operation
Input:  
(0,0) → 0
(0,1) → 1
(1,0) → 1
(1,1) → 0

Network Architecture:
Input layer: 2 neurons
Hidden layer: 3 neurons (sigmoid activation)
Output layer: 1 neuron (sigmoid activation)

Why XOR requires hidden layers?
XOR is not linearly separable. A single layer perceptron
cannot learn XOR because it can only represent linear
decision boundaries. A hidden layer allows the network
to learn non-linear combinations of inputs.
"""

  # Create network
  let network = Network(
    layers: @[
      newLayer(2, 3, ltSigmoid),
      newLayer(3, 1, ltSigmoid)
    ],
    learningRate: 0.5
  )

  # Training data
  let inputs = @[
    @[@[0.0, 0.0]],  # Input 1
    @[@[0.0, 1.0]],  # Input 2
    @[@[1.0, 0.0]],  # Input 3
    @[@[1.0, 1.0]]   # Input 4
  ]

  let targets = @[
    @[@[0.0]],  # Output 0
    @[@[1.0]],  # Output 1
    @[@[1.0]],  # Output 1
    @[@[0.0]]   # Output 0
  ]

  echo "\nTraining neural network on XOR problem..."
  echo "Epoch     Loss"
  echo "---------------"

  # Train for 1000 epochs
  for epoch in 0..<1000:
    var totalLoss = 0.0

    # Train on each example
    for idx in 0..<inputs.len:
      # Create input matrix
      var input = newMatrix(2, 1)
      input[0,0] = inputs[idx][0][0]
      input[1,0] = inputs[idx][0][1]

      # Create target matrix
      var target = newMatrix(1, 1)
      target[0,0] = targets[idx][0][0]

      # Compute gradients
      let gradients = network.backward(input, target)

      # Update parameters
      for i in 0..<network.layers.len:
        let (gradW, gradB) = gradients[i]
        network.layers[i].updateParams(gradW, gradB, network.learningRate)

      # Calculate loss
      let activations = network.forward(input)
      let output = activations[^1]
      let diff = output[0,0] - target[0,0]
      totalLoss += 0.5 * diff * diff

      # Clean up
      input.data.setLen(0)
      target.data.setLen(0)

    if epoch mod 100 == 0:
      echo &" {epoch:4d}    {totalLoss:.6f}"

  # Test the trained network
  echo "\nTrained Network Predictions:"
  echo "Input    Expected  Predicted"
  echo "-----    --------  ---------"

  for idx in 0..<inputs.len:
    var input = newMatrix(2, 1)
    input[0,0] = inputs[idx][0][0]
    input[1,0] = inputs[idx][0][1]

    let activations = network.forward(input)
    let output = activations[^1]

    echo &"({inputs[idx][0][0]:.0f},{inputs[idx][0][1]:.0f})" & "{targets[idx][0][0]:.0f}  {output[0,0]:.4f}"

    input.data.setLen(0)

proc performanceBenchmark() =
  echo "\n" & "=".repeat(70)
  echo "PERFORMANCE BENCHMARK (SIMD vs Scalar)"
  echo "=".repeat(70)

  let sizes = [64, 128, 256, 512]

  echo "\nMatrix Multiplication Performance (ms):"
  echo "Size    SIMD-Optimized"
  echo "----    --------------"

  for size in sizes:
    var a = randomMatrix(size, size, 0.1)
    var b = randomMatrix(size, size, 0.1)

    let start = getTime()
    var c = matMul(a, b)
    let elapsed = (getTime() - start).inMilliseconds.float()
    echo &"{size:4d} {elapsed:8.2f}"

    # Clean up
    a.data.setLen(0)
    b.data.setLen(0)
    c.data.setLen(0)

# ============================================
# Main Execution
# ============================================

when isMainModule:
  randomize()

  echo "\n" & "=".repeat(70)
  echo "DEEP BACKPROPAGATION WITH SIMD OPTIMIZATIONS"
  echo "=".repeat(70)

  when UseSimd:
    echo "\n✓ SIMD optimizations ENABLED (width = ", SimdWidth, ")"
  else:
    echo "\n✗ SIMD optimizations DISABLED (using scalar fallback)"

  # Explain mathematical principles
  explainBackpropagation()

  # Demonstrate matrix inversion
  demonstrateMatrixInversion()

  # Demonstrate backpropagation with XOR
  demonstrateBackpropagation()

  # Performance benchmark
  performanceBenchmark()

  echo "\n" & "=".repeat(70)
  echo "CONCLUSION"
  echo "=".repeat(70)
  echo """
Backpropagation is the fundamental algorithm for training neural networks:
1. Uses chain rule to compute gradients efficiently
2. Propagates errors backward through the network
3. Updates weights using gradient descent
4. SIMD optimization significantly accelerates matrix operations

Key Insights:
- The computational bottleneck is matrix multiplication (O(n³))
- Activation functions introduce non-linearity
- Hidden layers enable learning non-linear features
- Learning rate controls convergence speed
- Proper initialization prevents vanishing/exploding gradients
"""
