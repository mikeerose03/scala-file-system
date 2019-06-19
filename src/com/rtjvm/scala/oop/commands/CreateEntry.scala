package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

abstract class CreateEntry(name: String) extends Command {

  override def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(name)) state.setMessage(s"Entry $name already exists")
    else if (name.contains(Directory.SEPARATOR))
      state.setMessage(s"$name must not contain separators")
    else if (checkIllegal(name))
      state.setMessage(s"$name is an illegal entry name")
    else doCreateEntry(state, name)
  }

  def doCreateEntry (state: State, name: String): State = {
    def updateStructure(currentDir: Directory, path: List[String], newEntry: DirEntry): Directory = {
      if (path.isEmpty) currentDir.addEntry(newEntry)
      else {
        val oldEntry = currentDir.findEntry(path.head).asDirectory
        currentDir.replaceEntry(
          oldEntry.name,
          updateStructure(
            oldEntry,
            path.tail,
            newEntry
          )
        )
      }
    }

    val wd = state.wd
    // 1. get hold of all the directories in the fullpath
    val allDirsInPath = wd.getAllFoldersInPath
    // 2. create new directory entry in the wd
    val newEntry: DirEntry = createSpecificEntry(state)
    // 3. update the whole directory structure starting from the root
    // (the directory structure is IMMUTABLE)
    val newRoot = updateStructure(state.root, allDirsInPath, newEntry)
    // 4. find new working directory instance given wd's full path in the new directory structure

    val newWd = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWd)
  }

  def checkIllegal(name: String): Boolean = {
    name.contains(".")
  }

  def createSpecificEntry(state: State): DirEntry
}
