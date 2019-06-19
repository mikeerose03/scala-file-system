package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.Directory
import com.rtjvm.scala.oop.filesystem.State

class Rm(name: String) extends Command {

  override def apply(state: State): State = {
    // 1. get working directory
    val wd = state.wd

    // 2. get the absolute path
    val absolutePath =
      if(name.startsWith(Directory.SEPARATOR)) name
      else if(wd.isRoot) wd.path + name
      else wd.path + Directory.SEPARATOR + name

    // 3. do some validation
    if (Directory.ROOT_PATH.equals(absolutePath))
      state.setMessage("Nuclear War not supported yet.")
    else
      doRm (state, absolutePath)

  }

  def doRm(state: State, path: String): State = {

    def rmHelper(currentDirectory: Directory, path: List[String]): Directory = {
      if (path.isEmpty) currentDirectory
      else if (path.tail.isEmpty) currentDirectory.removeEntry(path.head)
      else {
        val nextDir = currentDirectory.findEntry(path.head)
        if (!nextDir.isDirectory) currentDirectory
        else {
          val newNextDir = rmHelper(nextDir.asDirectory, path.tail)
          if (newNextDir == nextDir) currentDirectory
          else currentDirectory.replaceEntry(path.head, newNextDir)
        }
      }
    }
    // 4. find the entry to remove
    // 5. update the structure like we did for mkdir

    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    val newRoot: Directory = rmHelper(state.root, tokens)

    if (newRoot == state.root)
      state.setMessage(s"$path does not exist")
    else
      State(newRoot, newRoot.findDescendant(state.wd.path.substring(1)))
  }
}
