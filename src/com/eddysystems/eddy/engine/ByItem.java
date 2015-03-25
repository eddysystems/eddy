package com.eddysystems.eddy.engine;

import com.eddysystems.eddy.EddyThread;
import com.intellij.psi.PsiField;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.ProjectScope;
import com.intellij.psi.search.PsiShortNamesCache;
import com.intellij.util.Processor;
import com.intellij.util.SmartList;
import com.intellij.util.indexing.IdFilter;
import scala.runtime.AbstractFunction0;
import tarski.*;
import tarski.Items.Item;
import tarski.Items.TypeItem;
import tarski.Items.Value;
import tarski.JavaScores.LazyBiased;
import tarski.Scores.Best;
import tarski.Scores.Empty$;
import tarski.Scores.Scored;

import java.util.*;

public class ByItem implements ValueByItemQuery {
  private final Map<TypeItem,Scored<Value>> cache = new HashMap<TypeItem,Scored<Value>>();
  private final Map<TypeItem,Value[]> locals;
  private final Converter converter;
  private final Map<String,Set<String>> globals;
  private final List<TypeNameItemNamePair> pairs;
  private final PsiShortNamesCache psiCache;
  private final GlobalSearchScope scope;
  private final IdFilter filter;
  private final EddyThread thread = EddyThread.getEddyThread();

  ByItem(Converter converter, Map<String,Set<String>> globals, List<TypeNameItemNamePair> pairs, List<Item> locals) {
    this.globals = globals;
    this.pairs = pairs;
    this.converter = converter;
    this.locals = JavaItems.valuesByItem(locals, true); // locals byItem map contains an entry for ObjectItem
    this.psiCache = PsiShortNamesCache.getInstance(converter.project);
    this.scope = ProjectScope.getProjectScope(converter.project);
    this.filter = IdFilter.getProjectIdFilter(converter.project, true);
  }

  public Scored<Value> query(final TypeItem type) {
    if (cache.containsKey(type))
      return cache.get(type);
    final String qual = type.qualified();

    // Avoid duplicates
    final Set<Value> seen = new HashSet<Value>();

    // Bias lookup into global map
    Scored<Value> s = new LazyBiased<Value>(Pr.globalByItem(),new AbstractFunction0<Scored<Value>>() {
      public Scored<Value> apply() {
        // Prepare to collect results
        final List<Value> results = new SmartList<Value>();
        final Processor<PsiField> proc = new Processor<PsiField>() {
          public boolean process(final PsiField field) {
            if (thread != null && thread.canceled())
              return false;
            final Value i = converter.addField(field);
            if (!seen.contains(i) && Types.isSubitem(i.item(),type))
              results.add(i);
            return true;
          }
        };

        // Check the current global map
        final Set<String> names = globals == null ? null : globals.get(qual);
        if (names != null) {
          if (thread != null) thread.pushSoftInterrupts();
          for (final String name : names)
            psiCache.processFieldsWithName(name, proc, scope, filter);
          if (thread != null) thread.popSoftInterrupts();
        }

        // Check changes since last global map was constructed
        for (final TypeNameItemNamePair tn : pairs)
          if (tn.typename.equals(qual)) {
            if (thread != null) thread.pushSoftInterrupts();
            psiCache.processFieldsWithName(tn.itemname, proc, scope, filter);
            if (thread != null) thread.popSoftInterrupts();
          }

        return JavaScores.uniformThen(JavaScores.one,results,(Scored)Empty$.MODULE$);
      }
    });

    // Add extraEnv
    final Value[] es = Base.extraByItem().get(type);
    if (es != null)
      for (int i=0;i<es.length;i++) {
        final Value x = es[i];
        s = new Best<Value>(JavaScores.one,x,s);
        seen.add(x);
      }

    // Add locals
    final Value[] ls = locals.get(type);
    if (ls != null)
      for (int i=0;i<ls.length;i++) {
        final Value x = ls[i];
        s = new Best<Value>(JavaScores.one,x,s);
        seen.add(x);
      }

    // All done!
    cache.put(type,s);
    return s;
  }
}
