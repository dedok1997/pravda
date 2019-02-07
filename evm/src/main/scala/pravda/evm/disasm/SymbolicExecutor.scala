package pravda.evm.disasm
import pravda.evm.EVM._
import pravda.evm.disasm.Blocks.{WithJumpDest, WithJumpI}

object SymbolicExecutor {

  case class SymbolicExecutionResult(jumps: Set[Jump], jumpis: Set[JumpI], path: List[Op])

  def evalOp(op: Op, state: StackWithNegativeSize[BigInt]): StackWithNegativeSize[BigInt] = {
    op match {
      case Push(n)                   => state push BigInt(1, n.toArray)
      case Swap(n) if n < state.size => state swap n
      case Dup(n) if n <= state.size => state dup n
      case And if state.size >= 2 && state.pop._1 >= Some(0) && state.pop._2.pop()._1 >= Some(0) =>
        val (Some(x), t) = state.pop()
        val (Some(y), res) = t.pop()
        res.push(x & y)

      case op =>
        val r = OpCodes.stackReadCount(op)
        val (args, s) = if (state.size >= r) state.pop(r) else state.pop(state.size)
        val w = OpCodes.stackWriteCount(op)
        val resState = (1 to w).foldLeft(s) { case (state, n) => state push BigInt(-n) }
        resState
    }
  }

  def eval(ops: Vector[Op]): Unit = {
    val jumpdest = ops.zipWithIndex.collect { case t @ (JumpDest(addr), ind) => addr -> ind }.toMap

    var used = Set.empty[Int]
    var usedI = Set.empty[Int]

    def eval(stack: StackWithNegativeSize[BigInt])(i: Int,
                                                   acc: SymbolicExecutionResult): List[SymbolicExecutionResult] = {
      usedI = usedI + i
      ops(i) match {
        case Return | Return(_) |  SelfDestruct | Stop | Invalid | Revert => List(acc)
        case SelfAddressedJump(addr) =>
          val (Some(to), state) = stack.pop()
          val ind = jumpdest(to.intValue())
          if (!used.contains(to.intValue())) {
            used = used + to.intValue()
            eval(state)(ind, acc.copy(jumps = acc.jumps + Jump(addr, to.intValue())))
          } else List(acc.copy(jumps = acc.jumps + Jump(addr, to.intValue())))
        case SelfAddressedJumpI(addr) =>
          val (Some(to), tail) = stack.pop()
          val (_, r) = tail.pop()
          val ind = jumpdest(to.intValue())
          if (!used.contains(to.intValue())) {
            used = used + to.intValue()
            eval(r)(ind, acc.copy(jumpis = acc.jumpis + JumpI(addr, to.intValue()))) ++ eval(r)(i + 1, acc)
          } else List(acc.copy(jumpis = acc.jumpis + JumpI(addr, to.intValue()))) ++ eval(r)(i + 1, acc)
        case op =>
          val state = evalOp(op, stack)
          eval(state)(i + 1, acc)
      }
  }
    val r = eval(StackList.empty)(0, SymbolicExecutionResult(Set.empty, Set.empty, Nil))

    val x = r.flatMap(t => t.jumps ++ t.jumpis).toSet
    val x1 = x.map(_.addr)
    val x2 = r.flatMap(t => t.jumps.map(_.dest) ++ t.jumpis.map(_.dest)).toSet
    val s = ops.zipWithIndex.filter{case (_,ind) => !usedI.contains(ind)}
    x -> x1 -> x2 -> s
    ???
  }

  def eval(main: List[List[Op]],
           withJumpDest: Map[Int, WithJumpDest],
           withJumpI: Map[Int, WithJumpI]): (Set[AddressedJumpOp], Set[WithJumpDest]) = {

    def eval(stack: StackWithNegativeSize[BigInt])(block: List[Op],
                                                   withJumpDest: Map[Int, WithJumpDest],
                                                   withJumpI: Map[Int, WithJumpI],
                                                   acc: SymbolicExecutionResult,
                                                   used: Set[Int]): List[SymbolicExecutionResult] = {
      val (st, lastOp) = block.foldLeft(stack -> Option.empty[Op]) {
        case ((stack, lastOp), op) =>
          op match {
            case SelfAddressedJump(addr) if stack.size > 0 =>
              val (Some(dest), r) = stack.pop()
              r -> Some(Jump(addr, dest.intValue()))
            case SelfAddressedJumpI(addr) if stack.size > 1 =>
              val (Some(dest), r) = stack.pop()
              val (_, res) = r.pop()
              res -> Some(JumpI(addr, dest.intValue()))
            case _ => evalOp(op, stack) -> lastOp
          }
      }

      lastOp match {

        case Some(j @ Jump(addr, dest)) if withJumpDest.contains(dest) && !used.contains(dest) =>
          eval(st)(
            withJumpDest(dest).ops,
            withJumpDest,
            withJumpI,
            acc.copy(jumps = acc.jumps + j, path = acc.path ++ block),
            used + dest
          )

        case Some(j @ JumpI(addr, dest)) if withJumpDest.contains(dest) && !used.contains(dest) =>
          val ex1 = eval(st)(
            withJumpDest(dest).ops,
            withJumpDest,
            withJumpI,
            acc.copy(jumpis = acc.jumpis + j, path = acc.path ++ block),
            used + dest
          )
          val ex2 = eval(st)(
            withJumpI(addr).ops,
            withJumpDest,
            withJumpI,
            acc.copy(jumpis = acc.jumpis + j, path = acc.path ++ block),
            used + dest
          )
          ex1 ++ ex2

        case _ => List(acc)
      }
    }

    val r = main.foldLeft(List.empty[SymbolicExecutionResult]) {
      case (acc, block) =>
        acc ++ eval(StackList.empty)(block,
                                     withJumpDest,
                                     withJumpI,
                                     SymbolicExecutionResult(Set.empty, Set.empty, Nil),
                                     Set.empty)
    }

    val r1 = withJumpDest.foldLeft(List.empty[SymbolicExecutionResult]) {
      case (acc, block) =>
        acc ++ eval(StackList.empty)(block._2.ops,
                                     withJumpDest,
                                     withJumpI,
                                     SymbolicExecutionResult(Set.empty, Set.empty, Nil),
                                     Set.empty)
    }

    val r2 = (r.map(res => res.jumps -> res.jumpis) ++ r1.map(res => res.jumps -> res.jumpis))
      .foldLeft(Set.empty[Jump] -> Set.empty[JumpI]) {
        case ((s1, s2), (s3, s4)) =>
          (s1 ++ s3) -> (s2 ++ s4)
      }

    val x: Set[AddressedJumpOp] = r2._1 ++ r2._2
    val y = r2._1.map(_.addr) ++ r2._2.map(_.addr)
    val z = r2._1.map(_.dest) ++ r2._2.map(_.dest)
    val yy = x.groupBy(_.addr).filter(_._2.size > 1)
    y -> z -> yy
    x -> Set.empty
  }

  def jumps(blocks: List[List[Op]]): (Set[AddressedJumpOp], Set[WithJumpDest]) = {
    val jumpable = Blocks.jumpable(blocks)
    val main = jumpable.withoutJumpdest.filter {
      case SelfAddressedJumpI(_) :: _ => false
      case _                          => true
    }
    val byJumpdest = jumpable.withJumpdest.groupBy(_.dest).map { case (k, v)     => k.addr -> v.head }
    val byJumpi = Blocks.continuation(blocks).groupBy(_.jumpi).map { case (k, v) => k.addr -> v.head }
    eval(main, byJumpdest, byJumpi)
  }
}
