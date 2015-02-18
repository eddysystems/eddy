package com.eddysystems.eddy.engine;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ContentIterator;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.roots.impl.ProjectFileIndexImpl;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.containers.ArrayListSet;
import com.intellij.util.indexing.FileBasedIndex;
import org.jetbrains.jps.model.java.JavaModuleSourceRootTypes;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class PackageIndex {
  final Project project;
  final private Map<String,Set<String>> toQualified = new HashMap<String, Set<String>>();

  PackageIndex(Project project) {
    this.project = project;
    // build toQualified by traversing directories
    //final GlobalSearchScope scope = new ProjectAndLibrariesScope(project);
    final ProjectRootManager prm = ProjectRootManager.getInstance(project);
    final ProjectFileIndexImpl idx = (ProjectFileIndexImpl)prm.getFileIndex();

    ContentIterator iter = new ContentIterator() {
      @Override
      public boolean processFile(VirtualFile fileOrDir) {
        if (fileOrDir.isDirectory()) {
          if (idx.isIgnored(fileOrDir))
            return true;

          // ignore resource files (some resource directories are just sitting in regular packages. Not
          // much we can do. Those will just never have classes in them.
          if (idx.isUnderSourceRootOfType(fileOrDir, JavaModuleSourceRootTypes.RESOURCES))
            return true;

          // source file
          VirtualFile root = idx.getSourceRootForFile(fileOrDir);
          // class file
          if (root == null)
            root = idx.getClassRootForFile(fileOrDir);

          // if not source, not class, or root, ignore
          if (root == null || root.getUrl().equals(fileOrDir.getUrl()))
            return true;

          String qpkg = fileOrDir.getPath().substring(root.getPath().length()).replace('/','.');

          // hacky, but we know no better way
          if (qpkg.isEmpty() || qpkg.equals(".") || qpkg.startsWith("META-INF"))
            return true;

          // non-jar classes end up with a '.' in front
          if (qpkg.charAt(0) == '.')
            qpkg = qpkg.substring(1);

          String pkg = qpkg.substring(qpkg.lastIndexOf('.')+1);

          Set<String> quals = toQualified.get(pkg);
          if (quals == null) {
            quals = new ArrayListSet<String>();
            toQualified.put(pkg,quals);
          }
          quals.add(qpkg);
        }
        return true;
      }
    };

    FileBasedIndex.getInstance().iterateIndexableFiles(iter, project, null);
  }

  // return all packages that have a qualified name something.or.other.shortName
  Set<String> get(String shortName) {
    Set<String> res = toQualified.get(shortName);
    return res != null ? res : Collections.<String>emptySet();
  }
}
