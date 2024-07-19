package boom.v4.exu

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config.Parameters
import boom.v4.common._

/** Find out what these do:
  * 
  * HasFPUParameters
  * FPUCtrlSigs
  * 
  * 
  * 
  */

// copied signals from HasFPUCtrlSigs
// -> find out what these all do
// and what is needed for vpu
trait HasVPUCtrlSigs {
  val ldst = Bool()
  val wen = Bool()
  val ren1 = Bool()
  val ren2 = Bool()
  val ren3 = Bool()
  val swap12 = Bool()
  val swap23 = Bool()
  val typeTagIn = UInt(2.W)
  val typeTagOut = UInt(2.W)
  val fromint = Bool()
  val toint = Bool()
  val fastpipe = Bool()
  val fma = Bool()
  val div = Bool()
  val sqrt = Bool()
  val wflags = Bool()
}

trait HasVPUParameters {
  // HasFPUParameters here:
  // https://github.com/chipsalliance/rocket-chip/blob/dbcb06afe1c76d1129cb6d264949322a34c37185/src/main/scala/tile/FPU.scala#L304
}

class VPUCtrlSigs extends Bundle with HasVPUCtrlSigs

/**
 * FP Decoder for the VPU
 */
class UOPCodeVPUDecoder(implicit p: Parameters) extends BoomModule with HasVPUParameters
{
  val io = IO(new Bundle {
    val uopc = Input(Bits(UOPC_SZ.W))
    val sigs = Output(new VPUCtrlSigs())
  })

  // values for control signals?
  // probably no, yes, don't care
  val X = BitPat("b?")
  val Y = BitPat("b1")
  val N = BitPat("b0")
  
  val default: List[BitPat] = List(X,X,X,X,X, X,X,X,X,X,X,X, X,X,X,X)

  // looks like this is a table to map uOPs to control signals?
  val instruction_table: Array[(BitPat, List[BitPat])] =
    // copied signals from FPUCtrlSigs
    //                                       swap12         fma
    //                                       | swap32       | div
    //                                       | | typeTagIn  | | sqrt
    //                            ldst       | | | typeTagOut | | wflags
    //                            | wen      | | | | from_int | | |
    //                            | | ren1   | | | | | to_int | | |
    //                            | | | ren2 | | | | | | fastpipe |
    //                            | | | | ren3 | | | | | |  | | | |
    //                            | | | | |  | | | | | | |  | | | |
    Array(
      BitPat(uopFNMSUB_S) -> List(X,X,Y,Y,Y, N,N,S,S,N,N,N, Y,N,N,Y)
    )

  val decoder = rocket.DecodeLogic(io.uopc, default, instruction_table)
}

/**
 * Bundle representing data to be sent to the VPU
 */
class VpuReq()(implicit p: Parameters) extends BoomBundle
{
  val uop      = new MicroOp()
  val rs1_data = Bits(65.W)
  val rs2_data = Bits(65.W)
  val rs3_data = Bits(65.W)
  val fcsr_rm  = Bits(tile.FPConstants.RM_SZ.W) // what is this?
}

class VPU(implicit p: Parameters) extends BoomModule with HasVPUParameters
{
  val io = IO(new Bundle {
    val req = Flipped(new ValidIO(new VpuReq))
    val resp = new ValidIO(new ExeUnitResp(65))
  })
}