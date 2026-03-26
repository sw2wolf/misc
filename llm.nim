###
# used to study Transformer Theory!
###
import std / [math, sequtils, random, strformat, times]

# ============================================
# Core Tensor Operations (Simplified)
# ============================================

type
  Tensor* = ref object
    data*: seq[float64]
    shape*: seq[int]

  Matrix* = seq[seq[float64]]

proc newTensor(shape: varargs[int]): Tensor =
  let size = shape.foldl(a * b, 1)
  result = Tensor(data: newSeq[float64](size), shape: @shape)

proc newTensor(data: seq[float64], shape: varargs[int]): Tensor =
  result = Tensor(data: data, shape: @shape)

proc `[]`(t: Tensor, indices: varargs[int]): float64 =
  # Simple indexing - assumes contiguous layout
  var idx = 0
  var stride = 1
  for i in countdown(indices.len - 1, 0):
    idx += indices[i] * stride
    if i > 0: stride *= t.shape[i]
  return t.data[idx]

proc `[]=`(t: Tensor, indices: varargs[int], val: float64) =
  var idx = 0
  var stride = 1
  for i in countdown(indices.len - 1, 0):
    idx += indices[i] * stride
    if i > 0: stride *= t.shape[i]
  t.data[idx] = val

# ============================================
# Attention Mechanism
# ============================================

type
  MultiHeadAttention* = ref object
    numHeads*: int
    headDim*: int
    scale*: float64
    wQ, wK, wV, wO: Matrix
    dropout: float64

proc softmax(x: seq[float64]): seq[float64] =
  let maxVal = x.foldl(max(a, b))
  let expVals = x.mapIt(exp(it - maxVal))
  let sumExp = expVals.sum()
  result = expVals.mapIt(it / sumExp)

proc matmul(a, b: Matrix): Matrix =
  result = newSeq[seq[float64]](a.len)
  for i in 0..<a.len:
    result[i] = newSeq[float64](b[0].len)
    for j in 0..<b[0].len:
      var sum = 0.0
      for k in 0..<a[0].len:
        sum += a[i][k] * b[k][j]
      result[i][j] = sum

#proc matmulVec(m: Matrix, v: seq[float64]): seq[float64] =
#  result = newSeq[float64](m.len)
#  for i in 0..<m.len:
#    var sum = 0.0
#    for j in 0..<v.len:
#      sum += m[i][j] * v[j]
#    result[i] = sum

proc transpose*(a: Matrix): Matrix =
  result = newSeq[seq[float64]](a[0].len)
  for i in 0..<a[0].len:
    result[i] = newSeq[float64](a.len)
    for j in 0..<a.len:
      result[i][j] = a[j][i]


proc scaledDotProductAttention(query, key, value: Matrix,
mask: seq[seq[float64]]): Matrix =
  # Q * K^T / sqrt(d_k)
  let dk = float64(query[0].len)
  var scores = matmul(query, transpose(key))

  # Scale
  for i in 0..<scores.len:
    for j in 0..<scores[0].len:
      scores[i][j] /= sqrt(dk)

  # Apply mask if provided
  if mask.len > 0:
    for i in 0..<scores.len:
      for j in 0..<scores[0].len:
        if mask[i][j] == 0:
          scores[i][j] = -1e9  # Mask out

  # Softmax along rows
  var attention = newSeq[seq[float64]](scores.len)
  for i in 0..<scores.len:
    attention[i] = softmax(scores[i])

  # Attention * V
  result = matmul(attention, value)

proc newMultiHeadAttention*(numHeads, headDim: int, dropout: float64 = 0.1): MultiHeadAttention =
  result = MultiHeadAttention(
    numHeads: numHeads,
    headDim: headDim,
    scale: 1.0 / sqrt(float64(headDim)),
    dropout: dropout
  )

  # Initialize weights (simplified - Xavier init would be better)
  let totalDim = numHeads * headDim

  # W_Q, W_K, W_V shapes: [totalDim, totalDim]
  result.wQ = newSeq[seq[float64]](totalDim)
  result.wK = newSeq[seq[float64]](totalDim)
  result.wV = newSeq[seq[float64]](totalDim)
  result.wO = newSeq[seq[float64]](totalDim)

  for i in 0..<totalDim:
    result.wQ[i] = newSeq[float64](totalDim)
    result.wK[i] = newSeq[float64](totalDim)
    result.wV[i] = newSeq[float64](totalDim)
    result.wO[i] = newSeq[float64](totalDim)
    for j in 0..<totalDim:
      result.wQ[i][j] = rand(0.1) - 0.05
      result.wK[i][j] = rand(0.1) - 0.05
      result.wV[i][j] = rand(0.1) - 0.05
      result.wO[i][j] = rand(0.1) - 0.05

proc forward*(mha: MultiHeadAttention, x: Matrix, mask: Matrix): Matrix =
  # x shape: [seq_len, d_model]
  let seqLen = x.len
  let dModel = x[0].len

  # Linear projections
  let q = matmul(x, mha.wQ)  # [seq_len, d_model]
  let k = matmul(x, mha.wK)
  let v = matmul(x, mha.wV)

  # Reshape for multi-head: [seq_len, num_heads, head_dim]
  let headDim = mha.headDim
  var qHeads = newSeq[seq[seq[float64]]](seqLen)
  var kHeads = newSeq[seq[seq[float64]]](seqLen)
  var vHeads = newSeq[seq[seq[float64]]](seqLen)

  for i in 0..<seqLen:
    qHeads[i] = newSeq[seq[float64]](mha.numHeads)
    kHeads[i] = newSeq[seq[float64]](mha.numHeads)
    vHeads[i] = newSeq[seq[float64]](mha.numHeads)

    for h in 0..<mha.numHeads:
      qHeads[i][h] = q[i][h*headDim ..< (h+1)*headDim]
      kHeads[i][h] = k[i][h*headDim ..< (h+1)*headDim]
      vHeads[i][h] = v[i][h*headDim ..< (h+1)*headDim]

  # Process each head
  var headOutputs = newSeq[seq[seq[float64]]](mha.numHeads)

  for h in 0..<mha.numHeads:
    # Extract head-specific matrices
    var qh = newSeq[seq[float64]](seqLen)
    var kh = newSeq[seq[float64]](seqLen)
    var vh = newSeq[seq[float64]](seqLen)

    for i in 0..<seqLen:
      qh[i] = qHeads[i][h]
      kh[i] = kHeads[i][h]
      vh[i] = vHeads[i][h]

    # Apply attention
    headOutputs[h] = scaledDotProductAttention(qh, kh, vh, mask)

  # Concatenate heads
  var concatenated = newSeq[seq[float64]](seqLen)
  for i in 0..<seqLen:
    concatenated[i] = newSeq[float64]()
    for h in 0..<mha.numHeads:
      concatenated[i].add(headOutputs[h][i])

  # Final linear projection
  result = matmul(concatenated, mha.wO)

# ============================================
# Positional Encoding
# ============================================

proc positionalEncoding*(seqLen, dModel: int): Matrix =
  result = newSeq[seq[float64]](seqLen)

  for pos in 0..<seqLen:
    result[pos] = newSeq[float64](dModel)
    for i in 0..<dModel:
      let angle = float64(pos) / pow(10000.0, float64(2 * i) / float64(dModel))
      if i mod 2 == 0:
        result[pos][i] = sin(angle)
      else:
        result[pos][i] = cos(angle)

# ============================================
# Feed-Forward Network
# ============================================

type
  FeedForward* = ref object
    w1, w2: Matrix
    activation: string  # "relu" or "gelu"

proc newFeedForward*(dModel, dff: int, activation: string = "relu"): FeedForward =
  result = FeedForward(activation: activation)

  # Initialize weights
  result.w1 = newSeq[seq[float64]](dModel)
  result.w2 = newSeq[seq[float64]](dff)

  for i in 0..<dModel:
    result.w1[i] = newSeq[float64](dff)
    for j in 0..<dff:
      result.w1[i][j] = rand(0.1) - 0.05

  for i in 0..<dff:
    result.w2[i] = newSeq[float64](dModel)
    for j in 0..<dModel:
      result.w2[i][j] = rand(0.1) - 0.05

proc forward*(ff: FeedForward, x: Matrix): Matrix =
  # First linear layer: [seq_len, d_model] -> [seq_len, d_ff]
  var hidden = matmul(x, ff.w1)

  # Activation
  for i in 0..<hidden.len:
    for j in 0..<hidden[0].len:
      if ff.activation == "relu":
        if hidden[i][j] < 0: hidden[i][j] = 0
      elif ff.activation == "gelu":
        # Approximate GELU
        hidden[i][j] = 0.5 * hidden[i][j] * (1 + tanh(sqrt(2/PI) * (hidden[i][j] + 0.044715 * pow(hidden[i][j], 3))))

  # Second linear layer: [seq_len, d_ff] -> [seq_len, d_model]
  result = matmul(hidden, ff.w2)

# ============================================
# Transformer Layer
# ============================================

type
  TransformerLayer* = ref object
    selfAttention: MultiHeadAttention
    feedForward: FeedForward
    norm1, norm2: string  # Layer norm placeholder
    dropout: float64

proc layerNorm(x: Matrix, epsilon: float64 = 1e-5): Matrix =
  # Simplified layer normalization
  result = newSeq[seq[float64]](x.len)
  for i in 0..<x.len:
    let mean = x[i].foldl(a + b) / float64(x[i].len)
    let variance = x[i].mapIt(pow(it - mean, 2)).foldl(a + b) / float64(x[i].len)
    let std = sqrt(variance + epsilon)
    result[i] = x[i].mapIt((it - mean) / std)

proc newTransformerLayer*(dModel, numHeads, dff: int, dropout: float64 = 0.1): TransformerLayer =
  result = TransformerLayer(
    selfAttention: newMultiHeadAttention(numHeads, dModel div numHeads, dropout),
    feedForward: newFeedForward(dModel, dff),
    dropout: dropout
  )

proc forward*(layer: TransformerLayer, x: Matrix, mask: Matrix): Matrix =
  # Self-attention with residual connection
  var attnOutput = layer.selfAttention.forward(x, mask)

  # Add & Norm
  var normalized1 = layerNorm(attnOutput)
  var residual1 = newSeq[seq[float64]](x.len)
  for i in 0..<x.len:
    residual1[i] = newSeq[float64](x[0].len)
    for j in 0..<x[0].len:
      residual1[i][j] = x[i][j] + normalized1[i][j]

  # Feed-forward with residual
  var ffOutput = layer.feedForward.forward(residual1)
  var normalized2 = layerNorm(ffOutput)

  # Final residual
  result = newSeq[seq[float64]](residual1.len)
  for i in 0..<residual1.len:
    result[i] = newSeq[float64](residual1[0].len)
    for j in 0..<residual1[0].len:
      result[i][j] = residual1[i][j] + normalized2[i][j]

# ============================================
# Complete Transformer Model
# ============================================

type
  Transformer* = ref object
    layers: seq[TransformerLayer]
    dModel: int
    vocabSize: int
    maxSeqLen: int
    tokenEmbedding: Matrix
    positionEmbedding: Matrix

proc newTransformer*(vocabSize, dModel, numLayers, numHeads, dff, maxSeqLen: int): Transformer =
  result = Transformer(
    dModel: dModel,
    vocabSize: vocabSize,
    maxSeqLen: maxSeqLen,
    tokenEmbedding: newSeq[seq[float64]](vocabSize),
    positionEmbedding: positionalEncoding(maxSeqLen, dModel)
  )

  # Initialize token embeddings
  for i in 0..<vocabSize:
    result.tokenEmbedding[i] = newSeq[float64](dModel)
    for j in 0..<dModel:
      result.tokenEmbedding[i][j] = rand(0.1) - 0.05

  # Create transformer layers
  result.layers = newSeq[TransformerLayer](numLayers)
  for i in 0..<numLayers:
    result.layers[i] = newTransformerLayer(dModel, numHeads, dff)

proc forward*(model: Transformer, inputIds: seq[int], mask: Matrix): Matrix =
  let seqLen = inputIds.len
  assert seqLen <= model.maxSeqLen

  # Embedding layer: token embeddings + positional embeddings
  var x = newSeq[seq[float64]](seqLen)
  for i in 0..<seqLen:
    let tokenId = inputIds[i]
    assert tokenId < model.vocabSize
    x[i] = newSeq[float64](model.dModel)
    for j in 0..<model.dModel:
      x[i][j] = model.tokenEmbedding[tokenId][j] + model.positionEmbedding[i][j]

  # Pass through transformer layers
  for layer in model.layers:
    x = layer.forward(x, mask)

  result = x

# ============================================
# Simple Training Example
# ============================================

proc crossEntropyLoss(predictions, targets: seq[int]): float64 =
  # Simplified cross-entropy loss
  result = 0.0
  for i in 0..<predictions.len:
    if predictions[i] != targets[i]:
      result += 1.0
  result /= float64(predictions.len)

proc generateCausalMask(seqLen: int): Matrix =
  result = newSeq[seq[float64]](seqLen)
  for i in 0..<seqLen:
    result[i] = newSeq[float64](seqLen)
    for j in 0..<seqLen:
      result[i][j] = if j <= i: 1.0 else: 0.0

# ============================================
# Demonstration
# ============================================

proc main() =
  echo "=== Transformer Model from Scratch ==="
  echo ""

  # Model parameters
  let vocabSize = 1000
  let dModel = 128
  let numLayers = 4
  let numHeads = 8
  let dff = 512
  let maxSeqLen = 50

  echo "Creating Transformer model..."
  echo &"  Vocabulary size: {vocabSize}"
  echo &"  Model dimension: {dModel}"
  echo &"  Number of layers: {numLayers}"
  echo &"  Attention heads: {numHeads}"
  echo &"  Feed-forward dimension: {dff}"
  echo ""

  let model = newTransformer(vocabSize, dModel, numLayers, numHeads, dff, maxSeqLen)

  # Example input: sequence of token IDs
  let inputSequence = @[1, 2, 3, 4, 5]
  let causalMask = generateCausalMask(inputSequence.len)

  echo "Processing input sequence:"
  echo &"  Input IDs: {inputSequence}"
  echo ""

  # Forward pass
  let startTime = cpuTime()
  let output = model.forward(inputSequence, causalMask)
  let elapsed = cpuTime() - startTime

  echo &"Forward pass completed in {elapsed:.4f} seconds"
  echo &"Output shape: [{output.len}, {output[0].len}]"
  echo ""

  # Show some output statistics
  echo "Output statistics (first token, first 10 dimensions):"
  for i in 0..<min(10, output[0].len):
    echo &"  dim {i}: {output[0][i]:.4f}"
  echo ""

  echo "=== Key Concepts Demonstrated ==="
  echo "1. Self-Attention: Query, Key, Value projections"
  echo "2. Multi-Head Attention: Parallel attention mechanisms"
  echo "3. Positional Encoding: Sinusoidal position information"
  echo "4. Feed-Forward Networks: MLP with activation functions"
  echo "5. Residual Connections: Skip connections for gradient flow"
  echo "6. Layer Normalization: Stabilizes training"
  echo "7. Causal Masking: Prevents looking at future tokens"
  echo ""
  echo "This implementation shows the core mathematics"
  echo "behind the Transformer architecture from the paper"
  echo "\"Attention Is All You Need\" (Vaswani et al., 2017)"

when isMainModule:
  randomize()
  main()
