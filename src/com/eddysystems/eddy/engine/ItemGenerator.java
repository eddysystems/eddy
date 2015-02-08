package com.eddysystems.eddy.engine;

import com.eddysystems.eddy.EddyThread;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiField;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.PsiShortNamesCache;
import com.intellij.util.Processor;
import com.intellij.util.indexing.IdFilter;
import org.jetbrains.annotations.NotNull;
import tarski.Items;
import tarski.JavaTrie.Generator;

import java.util.ArrayList;
import java.util.List;

class ItemGenerator implements Generator<Items.Item> {

  static final int cacheSize = 10000;
  final LRUCache<String, Items.Item[]> cache = new LRUCache<String, Items.Item[]>(cacheSize);

  final Project project;
  final GlobalSearchScope scope;
  final PsiShortNamesCache psicache;
  final IdFilter filter = new IdFilter() { @Override public boolean containsFileId(int id) { return true; } };
  final Converter converter;

  ItemGenerator(Project project, GlobalSearchScope scope, Converter conv) {
    this.project = project;
    this.scope = scope;
    this.psicache = PsiShortNamesCache.getInstance(project);
    converter = conv;
  }

  private Items.Item[] generate(String s) {
    final EddyThread thread = EddyThread.getEddyThread();
    final List<Items.Item> results = new ArrayList<Items.Item>();

    final Processor<PsiClass> classProc = new Processor<PsiClass>() {
      @Override
      public boolean process(PsiClass cls) {
        if (thread != null && thread.canceled())
          return false;
        results.add(converter.addClass(cls));
        return true;
      }
    };

    final Processor<PsiMethod> methodProc = new Processor<PsiMethod>() {
      @Override
      public boolean process(PsiMethod method) {
        if (thread != null && thread.canceled())
          return false;
        results.add(converter.addMethod(method));
        return true;
      }
    };

    final Processor<PsiField> fieldProc = new Processor<PsiField>() {
      @Override
      public boolean process(PsiField fld) {
        if (thread != null && thread.canceled())
          return false;
        results.add(converter.addField(fld));
        return true;
      }
    };

    if (thread != null) thread.pushSoftInterrupts();
    try {
      psicache.processClassesWithName(s, classProc, scope, filter);
      psicache.processMethodsWithName(s, methodProc, scope, filter);
      psicache.processFieldsWithName(s, fieldProc, scope, filter);
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
