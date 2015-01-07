package com.eddysystems.eddy;

import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.Map;
import java.util.PriorityQueue;

class LRUCache<K,V> {

  private class Entry {
    long timestamp;
    V result;
    Entry(long timestamp, V result) {
      this.timestamp = timestamp;
      this.result = result;
    }
  }

  private class Query implements Comparable<Query> {
    long timestamp;
    K s;

    Query(long timestamp, K s) {
      this.timestamp = timestamp;
      this.s = s;
    }

    @Override
    public int compareTo(Query o) {
      return (int)(timestamp - o.timestamp);
    }
  }

  private long timestamp = 0;

  private final PriorityQueue<Query> lru = new PriorityQueue<Query>();
  private final Map<K,Entry> cache = new HashMap<K,Entry>();

  private final int maxSize;

  LRUCache(int size) {
    this.maxSize = size;
  }

  private void clean() {
    while (cache.size() > maxSize) {
      cache.remove(lru.poll().s);
    }
  }

  public @Nullable
  V get(K s) {
    if (!cache.containsKey(s))
      return null;

    Entry e = cache.get(s);
    Query q = new Query(e.timestamp,s);
    lru.remove(q);
    q.timestamp = timestamp;
    lru.add(q);
    e.timestamp = timestamp;
    return e.result;
  }

  public void put(K s, V v) {
    cache.put(s, new Entry(timestamp, v));
    lru.add(new Query(timestamp,s));
    clean();
  }

}
