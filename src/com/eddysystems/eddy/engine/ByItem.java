package com.eddysystems.eddy.engine;

import com.eddysystems.eddy.EddyThread;
import com.intellij.psi.PsiField;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.PsiType;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.ProjectScope;
import com.intellij.psi.search.PsiShortNamesCache;
import com.intellij.util.Processor;
import com.intellij.util.SmartList;
import com.intellij.util.indexing.IdFilter;
import scala.runtime.AbstractFunction0;
import tarski.*;
import tarski.Items.*;
import tarski.JavaItems.*;
import tarski.JavaScores.LazyBiased;
import tarski.Scores.Best;
import tarski.Scores.Empty$;
import tarski.Scores.Scored;
import utility.Interrupts;

import java.util.*;
import static tarski.Flags.nullaryMethods;

public class ByItem implements ValueByItemQuery {
  private final Map<TypeItem,Scored<ValueOrMethod>> cache = new HashMap<TypeItem,Scored<ValueOrMethod>>();
  private final Converter converter;
  private final Map<TypeItem,Value[]> localValues;
  private final Map<TypeItem,MethodItem[]> localMethods;
  private final Map<String,Set<String>> globalFields, globalMethods;
  private final List<TypeNameItemNamePair> pairFields, pairMethods;
  private final PsiShortNamesCache psiCache;
  private final GlobalSearchScope scope;
  private final IdFilter filter;
  private final EddyThread thread = EddyThread.getEddyThread();

  ByItem(final Converter converter,
         final Map<String,Set<String>> globalFields, final Map<String,Set<String>> globalMethods,
         final List<TypeNameItemNamePair> pairFields, final List<TypeNameItemNamePair> pairMethods,
         final List<Item> locals,
         final GlobalSearchScope scope, final IdFilter filter) {
    this.converter = converter;
    final ByItemMaps maps = JavaItems.valuesByItem(locals, true); // locals byItem map contains an entry for ObjectItem
    this.localValues = maps.values;
    this.localMethods = maps.methods;
    this.globalFields = globalFields;
    this.globalMethods = globalMethods;
    this.pairFields = pairFields;
    this.pairMethods = pairMethods;
    this.psiCache = PsiShortNamesCache.getInstance(converter.project);
    this.scope = scope;
    this.filter = filter;
  }

  // Is a method suitable for by item lookup?
  // IMPORTANT: Must match JavaItems.considerMethod
  static public boolean considerMethod(final PsiMethod m, final PsiType ret) {
    return ret != null && ret != PsiType.VOID && considerMethodHelper(m);
  }
  static public boolean considerMethodHelper(final PsiMethod m) { // Do the non-type checks
    return !m.hasTypeParameters() && m.getParameterList().getParametersCount() == 0;
  }

  public Scored<ValueOrMethod> query(final TypeItem type) {
    if (cache.containsKey(type))
      return cache.get(type);
    final String qual = type.qualified();

    // Avoid duplicates
    final Set<ValueOrMethod> seen = new HashSet<ValueOrMethod>();

    // Bias lookup into global map
    Scored<ValueOrMethod> s = new LazyBiased<ValueOrMethod>(Pr.globalByItem(),new GlobalFields(qual,type,seen));

    // Add extra values
    {
      final Value[] es = Base.extraByItem().values.get(type);
      if (es != null)
        for (int i=0;i<es.length;i++) {
          final Value x = es[i];
          s = new Best<ValueOrMethod>(JavaScores.one,x,s);
          seen.add(x);
        }
    }

    // Add local values
    {
      final Value[] ls = localValues.get(type);
      if (ls != null)
        for (int i=0;i<ls.length;i++) {
          final Value x = ls[i];
          s = new Best<ValueOrMethod>(JavaScores.one,x,s);
          seen.add(x);
        }
    }

    // Add local methods
    if (nullaryMethods) {
      Scored<ValueOrMethod> m = (Scored)Empty$.MODULE$;
      final MethodItem[] ls = localMethods.get(type);
      if (ls != null)
        for (int i=0;i<ls.length;i++) {
          final MethodItem x = ls[i];
          m = new Best<ValueOrMethod>(Pr.methodByItem(),x,m);
          seen.add(x);
        }
      s = s.$plus$plus(m);
    }

    // All done!
    cache.put(type,s);
    return s;
  }

  private final class GlobalFields extends AbstractFunction0<Scored<ValueOrMethod>> {
    final String qual;
    final TypeItem type;
    final Set<ValueOrMethod> seen;

    GlobalFields(final String qual, final TypeItem type, final Set<ValueOrMethod> seen) {
      this.qual = qual;
      this.type = type;
      this.seen = seen;
    }

    public Scored<ValueOrMethod> apply() {
      // Prepare to collect values
      final List<Value> results = new SmartList<Value>();
      final Processor<PsiField> proc = new Processor<PsiField>() {
        public boolean process(final PsiField f) {
          if (thread != null && thread.canceled())
            return false;
          final Value i = converter.addField(f);
          if (!seen.contains(i))
            results.add(i);
          return true;
        }
      };

      // Check the current global map
      final Set<String> names = globalFields == null ? null : globalFields.get(qual);
      if (names != null) {
        if (thread != null) thread.pushSoftInterrupts();
        for (final String name : names)
          psiCache.processFieldsWithName(name, proc, scope, filter);
        if (thread != null) thread.popSoftInterrupts();
      }

      // Check changes since last global map was constructed
      for (final TypeNameItemNamePair tn : pairFields)
        if (tn.type.equals(qual)) {
          if (thread != null) thread.pushSoftInterrupts();
          psiCache.processFieldsWithName(tn.item, proc, scope, filter);
          if (thread != null) thread.popSoftInterrupts();
        }

      // Filter out values that aren't subitems of our desired type.
      // Putting this logic in proc might cause deadlocks since isSubitem can trigger Scala plugin code.
      final int fullSize = results.size();
      int size = fullSize;
      for (int i=size-1;i>=0;i--) {
        if (Interrupts.pending != 0) Interrupts.checkInterrupts();
        if (!Types.isSubitem(results.get(i).item(), type))
          results.set(i, results.get(--size));
      }
      if (size != fullSize)
        results.subList(size, fullSize).clear();

      // All done, except for methods
      final Scored<MethodItem> methods = !nullaryMethods ? (Scored<MethodItem>)(Scored)Empty$.MODULE$
        : new LazyBiased<MethodItem>(Pr.methodByItem(),new GlobalMethods(qual,type,seen));
      return JavaScores.uniformThen(JavaScores.one,results,(Scored<ValueOrMethod>)(Scored)methods);
    }
  }

  private final class GlobalMethods extends AbstractFunction0<Scored<MethodItem>> {
    final String qual;
    final TypeItem type;
    final Set<ValueOrMethod> seen;

    GlobalMethods(final String qual, final TypeItem type, final Set<ValueOrMethod> seen) {
      this.qual = qual;
      this.type = type;
      this.seen = seen;
    }

    public Scored<MethodItem> apply() {
      // Prepare to collect results
      final List<MethodItem> results = new SmartList<MethodItem>();
      final Processor<PsiMethod> proc = new Processor<PsiMethod>() {
        public boolean process(final PsiMethod m) {
          if (thread != null && thread.canceled())
            return false;
          if (!considerMethodHelper(m))
            return true;
          final MethodItem i = (MethodItem)converter.addMethod(m);
          if (!seen.contains(i))
            results.add(i);
          return true;
        }
      };

      // Check the current global map
      final Set<String> names = globalMethods == null ? null : globalMethods.get(qual);
      if (names != null) {
        if (thread != null) thread.pushSoftInterrupts();
        for (final String name : names)
          psiCache.processMethodsWithName(name, proc, scope, filter);
        if (thread != null) thread.popSoftInterrupts();
      }

      // Check changes since last global map was constructed
      for (final TypeNameItemNamePair tn : pairMethods)
        if (tn.type.equals(qual)) {
          if (thread != null) thread.pushSoftInterrupts();
          psiCache.processMethodsWithName(tn.item, proc, scope, filter);
          if (thread != null) thread.popSoftInterrupts();
        }

      // Filter out values that aren't subitems of our desired type.
      // Putting this logic in proc might cause deadlocks since isSubitem can trigger Scala plugin code.
      final int fullSize = results.size();
      int size = fullSize;
      for (int i=size-1;i>=0;i--) {
        if (Interrupts.pending != 0) Interrupts.checkInterrupts();
        final MethodItem m = results.get(i);
        final TypeItem ty = m.retItem();
        if (!JavaItems.considerMethod(m,ty) || !Types.isSubitem(ty,type))
          results.set(i, results.get(--size));
      }
      if (size != fullSize)
        results.subList(size, fullSize).clear();

      return JavaScores.uniformThen(JavaScores.one,results,(Scored<MethodItem>)(Scored)Empty$.MODULE$);
    }
  }
}
