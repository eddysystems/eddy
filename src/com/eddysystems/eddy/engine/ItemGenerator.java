package com.eddysystems.eddy.engine;

import com.eddysystems.eddy.EddyThread;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiField;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.impl.file.PsiPackageImpl;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.PsiShortNamesCache;
import com.intellij.util.Processor;
import com.intellij.util.indexing.IdFilter;
import org.jetbrains.annotations.NotNull;
import tarski.Items;
import tarski.JavaTrie.Generator;

import java.util.HashSet;

class ItemGenerator implements Generator<Items.Item> {

  static final int cacheSize = 10000;
  final LRUCache<String, Items.Item[]> cache = new LRUCache<String, Items.Item[]>(cacheSize);

  final Project project;
  final GlobalSearchScope scope;
  final IdFilter filter;
  final PsiShortNamesCache psiCache;
  final PsiManager psiManager;
  final Converter converter;
  final PackageIndex packageIndex;

  ItemGenerator(Project project, GlobalSearchScope scope, Converter conv, PackageIndex packageIndex) {
    this.project = project;
    this.scope = scope;
    filter = IdFilter.getProjectIdFilter(project, true);
    this.psiCache = PsiShortNamesCache.getInstance(project);
    psiManager = PsiManager.getInstance(project);
    converter = conv;
    this.packageIndex = packageIndex;
  }

  private Items.Item[] generate(String s) {
    final EddyThread thread = EddyThread.getEddyThread();
    final HashSet<Items.Item> results = new HashSet<Items.Item>();

    // find and add packages
    for (String pkgQualifiedName: packageIndex.get(s)) {
      Items.Package pkg = (Items.Package) converter.addContainer(new PsiPackageImpl(psiManager, pkgQualifiedName));
      results.add(pkg);
    }

    final Processor<PsiClass> classProc = new Processor<PsiClass>() {
      @Override
      public boolean process(PsiClass cls) {
        // TODO: Ideally we'd check for interrupts here, but can't because our caller grabs fancy locks
        if (thread != null && thread.canceled()) return false;
        results.add(converter.addClass(cls));
        return true;
      }
    };

    final Processor<PsiMethod> methodProc = new Processor<PsiMethod>() {
      @Override
      public boolean process(PsiMethod method) {
        // TODO: Ideally we'd check for interrupts here, but can't because our caller grabs fancy locks
        if (thread != null && thread.canceled()) return false;
        if (!method.isConstructor())
          results.add(converter.addMethod(method));
        return true;
      }
    };

    final Processor<PsiField> fieldProc = new Processor<PsiField>() {
      @Override
      public boolean process(PsiField fld) {
        // TODO: Ideally we'd check for interrupts here, but can't because our caller grabs fancy locks
        if (thread != null && thread.canceled()) return false;
        results.add(converter.addField(fld));
        return true;
      }
    };

    if (thread != null) thread.pushSoftInterrupts();
    try {
      final IdFilter filter = null;
      psiCache.processClassesWithName(s, classProc, scope, filter);
      psiCache.processMethodsWithName(s, methodProc, scope, filter);
      psiCache.processFieldsWithName(s, fieldProc, scope, filter);
    } finally {
      if (thread != null) thread.popSoftInterrupts();
    }
    return results.toArray(new Items.Item[results.size()]);
  }

  @Override @NotNull
  public Items.Item[] lookup(String s) {
    Items.Item[] result = cache.get(s);

    if (result != null)
      return result;
    else
      result = generate(s);

    // add to cache
    cache.put(s, result);
    return result;
  }
}
