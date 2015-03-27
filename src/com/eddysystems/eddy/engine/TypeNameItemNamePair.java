package com.eddysystems.eddy.engine;

import org.jetbrains.annotations.NotNull;

public class TypeNameItemNamePair {
  @NotNull final public String type;
  @NotNull final public String item;

  public String toString() {
    return type + ' ' + item;
  }

  TypeNameItemNamePair(String type, String item) {
    this.type = type;
    this.item = item;
  }
}
