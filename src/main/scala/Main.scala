import compiler.Compiler

object Main extends App {

  if (args.length == 0) {
    sys.exit(1)
  }

  val path = args(0)
  Compiler.compileAndExit(path)

}
