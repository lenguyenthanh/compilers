//> using scala "3.1.3"
//> using lib "com.lihaoyi::os-lib:0.8.0"

import scala.util.Properties

val platformSuffix: String = {
  val os =
    if (Properties.isWin) "pc-win32"
    else if (Properties.isLinux) "pc-linux"
    else if (Properties.isMac) "apple-darwin"
    else sys.error(s"Unrecognized OS: ${sys.props("os.name")}")
  os
}
val artifactsPath = os.Path("artifacts", os.pwd)

def destPath(prefix: String) =
  if (Properties.isWin) artifactsPath / s"$prefix-ulc-$platformSuffix.exe"
  else artifactsPath / s"$prefix-ulc-$platformSuffix"

val scalaCLILauncher =
  if (Properties.isWin) "scala-cli.bat" else "scala-cli"

os.makeDir(artifactsPath)
os.proc(scalaCLILauncher, "package", "./ulc", "-o", destPath("graal"), "--native-image")
  .call(cwd = os.pwd)
  .out
  .text()
  .trim

os.proc(scalaCLILauncher, "package", "./ulc", "-o", destPath("native"), "--native", "--native-version", "0.4.5")
  .call(cwd = os.pwd)
  .out
  .text()
  .trim
