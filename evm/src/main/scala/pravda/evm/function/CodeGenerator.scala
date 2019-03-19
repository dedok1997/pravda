package pravda.evm.function

import pravda.evm.translate.opcode._
import pravda.vm.{Data, Opcodes, asm}
import pravda.vm.asm.Operation

//
//
//object TypedCodeGenerator{
//
//  import shapeless._
//  import shapeless.ops.hlist._
//  import shapeless.ops.nat._
//  import shapeless.nat._
//  trait EvmWord
//
//  case object Stub extends EvmWord
//
//  def push[T <: HList,E](value: E,toPrimitive: E => Data.Primitive, l: List[Operation], stack: T): (EvmWord :: T,List[Operation]) =
//    (Stub :: stack) -> (l ++ (asm.Operation.Push(toPrimitive(value)) :: Nil))
//
//  def pop[T <: HList,N <: Nat](l: List[Operation], stack: T)(implicit
//                               len: Length.Aux[T, N],
//                               c: IsHCons[T],
//                               nat: shapeless.ops.nat.GTEq[N,_1]
//                              ) ={
//    len -> nat
//    stack.tail -> (l ++ CodeGenerator.pop)
//  }
//
////  def dup[H <: HList,N <: Nat](n: N,l: List[Operation],stack: H)(implicit
////                                         toInt: ToInt[n.N],
////                                         //gt: GT[n.N,_0],
////                                          m: shapeless.ops.nat.Diff[N,_1],
////                                         at: At[H, shapeless.ops.nat.Diff[N,_1]])  = {
////    val x = stack[shapeless.ops.nat.Diff[N,_1]] : at.Out
////    (x :: stack) -> (l ++ CodeGenerator.dup(toInt()))
////  }
//
//
//  def swap[H, L <: HList, N <: Nat, P <: HList, X, S <: HList](n: Succ[N],l: H :: L,l1: List[Operation])(
//                                                                implicit
//                                                                toInt: ToInt[N],
//                                                                split: Lazy[Split.Aux[L, N, P, X :: S]],
//                                                                prepend: Prepend[P, H :: S]
//                                                              ) = {
//    val (p, x :: s) = split.value(l.tail)
//    (x :: (p ++ (l.head :: s))) -> (l1 ++ CodeGenerator.swap(toInt()))
//  }
//
//}


object CodeGenerator {

  implicit class CodeAdderByOps(ops: List[Operation])
  {
    def ~(ops1: List[Operation]): List[Operation] = ops ++ ops1
    def ~(op: Operation): List[Operation] = ops ++ List(op)
  }

  implicit class CodeAdderByOp(op1: Operation)
  {
    def ~(ops: List[Operation]): List[Operation] = op1 :: ops
    def ~(op2: Operation): List[Operation] = List(op1,op2)
  }


  /*
    input:
      stack:
        0: offset: bigint
        ..
        (stackSize + 1): refToMemory
    output:
      stack:
       0: mem[offset : offset + length]
  */
  def readFromMemory(stackSize: Int,length: Int): List[Operation] =
    pushBigInt(scala.BigInt(length)) ~ codeToOps(Opcodes.SWAP) ~ readFromMemory(stackSize + 1)

  /*
    input:
      stack:
        0: offset: bytes | bigint
        1: length: bytes | bigint
        ..
        (stackSize + 1): refToMemory
    output:
      stack:
       0: mem[offset : offset + length]
  */
  def readFromMemory(stackSize: Int): List[Operation] =
      cast(Data.Type.BigInt) ~
      swap(1) ~ cast(Data.Type.BigInt) ~ swap(1) ~
      dup(stackSize + 1) ~
      scall(6)


  /*
     input:
      stack:
       0: offset: bigint
       1: value: bytes
        ..
        (stackSize + 1): refToMemory
     output:
      stack:
        ---
   */
  def writeToMemory(stackSize: Int): List[Operation] = {
    codeToOps(Opcodes.SWAP) ~ List(pushInt8(8)) ~ codeToOps(Opcodes.SCALL, Opcodes.SWAP) ~
      cast(Data.Type.BigInt) ~ pushInt(stackSize + 1) ~ codeToOps(Opcodes.DUPN) ~
      scall(7) ~
      pushInt(stackSize) ~ codeToOps(Opcodes.SWAPN, Opcodes.POP)
  }


  def writeToMemoryWithoutExpand(stackSize: Int): List[Operation] = {
      cast(Data.Type.BigInt) ~ pushInt(stackSize + 1) ~ codeToOps(Opcodes.DUPN) ~
      List(pushInt8(7), Operation(Opcodes.SCALL)) ~
      pushInt(stackSize) ~ codeToOps(Opcodes.SWAPN, Opcodes.POP)
  }


  /*
    input:
      stack:
        0: offset: bigint
        ..
        (stackSize + 2): inputData: Bytes
    output:
      stack:
        0: inputData[offset : offset + length]
  */
  def readFromInputData(stackSize: Int,length: Int): List[Operation] = {
    cast(Data.Type.BigInt) :::
      pushInt(stackSize + 2) ::
      codeToOps(Opcodes.DUPN, Opcodes.SWAP, Opcodes.DUP) :::
      pushInt(length) ::
      codeToOps(Opcodes.ADD, Opcodes.SWAP, Opcodes.SLICE)
  }

  def label(name: String): List[Operation] = Operation.Label(name) :: Nil
  def push(d: Data.Primitive): List[Operation] = Operation.Push(d) :: Nil
  def jumpi(label: Option[String]): List[Operation] = Operation.JumpI(label) :: Nil
  def scall(n: Byte) = List(Operation.Push(Data.Primitive.Int8(n)), Operation(Opcodes.SCALL))

  def dup(n: Int): List[Operation] = if (n > 1) dupn(n) else codeToOps(Opcodes.DUP)
  def swap(n: Int): List[Operation] = if (n > 1) swapn(n + 1) else codeToOps(Opcodes.SWAP)
  val pop: List[Operation] = asm.Operation(Opcodes.POP) :: Nil
  val eq: List[Operation] = codeToOps(Opcodes.EQ)
  val not: List[Operation] = codeToOps(Opcodes.NOT)
  val pcall: List[Operation] = codeToOps(Opcodes.PCALL)
  val ret: List[Operation] = codeToOps(Opcodes.RET)
  val stop: List[Operation] = codeToOps(Opcodes.STOP)
  val length: List[Operation] = codeToOps(Opcodes.LENGTH)
  val gt: List[Operation] = codeToOps(Opcodes.GT)
  val expand = scall(8)
  val sliceArray = scall(6)
  val slice = codeToOps(Opcodes.SLICE)



}
