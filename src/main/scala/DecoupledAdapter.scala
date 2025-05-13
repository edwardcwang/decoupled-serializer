package decoupledserializer

import chisel3._
import chisel3.util.Decoupled

/** Divides up a `inputWidth`-bit message into `outputWidth`-bits.
  * It will send out `outputWidth`-bits per cycle, little endian.
  * For an input to be fully processed, all parts of the output must be consumed.
  */
class Serializer(val inputWidth: Int, val outputWidth: Int) extends Module {
  require(inputWidth >= 1 && outputWidth >= 1, "Must have positive width")
  require(inputWidth % outputWidth == 0, "Input width must be a multiple of output width")

  val input = IO(Flipped(Decoupled(UInt(inputWidth.W))))
  val output = IO(Decoupled(UInt(outputWidth.W)))

  // Number of parts of the output.
  val numParts: Int = inputWidth / outputWidth
  assert(numParts >= 1)

  // Are we currently emitting data to the output?
  val emitting = RegInit(false.B)
  // Cache of input data
  val inputCache = RegInit(0.U(inputWidth.W))
  // Number of parts left to emit
  val currentPart = RegInit(0.U(chisel3.util.unsignedBitLength(numParts + 1).W))

  // Output is valid if we are emitting and have any chunks left
  val thisPartValid = currentPart < numParts.U
  val nextPartValid = (currentPart + 1.U) < numParts.U
  output.valid := emitting && thisPartValid
  // Emit each outputWidth-sized chunk
  output.bits := (inputCache >> (currentPart * outputWidth.U))(outputWidth - 1, 0)

  // Ready for input if not emitting
  input.ready := ~emitting

  when(~emitting) {
    // Not emitting data.
    // If there is data to ingest, ingest it.
    when(input.fire) {
      emitting := true.B
      inputCache := input.bits
      currentPart := 0.U
    }
  }.otherwise {
    // Emitting data
    when(output.fire) {
      currentPart := currentPart + 1.U
      // Turn off emitting flag if we are out of parts
      emitting := nextPartValid
    }
  }
}

/** This is a **combinational** demuxer which routes the input message to the particular output port depending on its LSB.
  * If LSB=0, then route to port 0; else if LSB=1, route to port 1.
  */
class Demuxer(val width: Int) extends RawModule {
  val in = IO(Flipped(Decoupled(UInt(width.W))))
  val out0 = IO(Decoupled(UInt(width.W)))
  val out1 = IO(Decoupled(UInt(width.W)))

  out0.valid := in.valid && (~in.bits(0))
  out1.valid := in.valid && in.bits(0)

  out0.bits := in.bits
  out1.bits := in.bits

  in.ready := Mux(in.bits(0), out1.ready, out0.ready)
}
