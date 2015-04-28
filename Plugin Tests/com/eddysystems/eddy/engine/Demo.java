/* Demo: Definitions for use when demoing eddy */

package com.eddysystems.eddy.engine;

// need these

import com.intellij.openapi.diagnostic.Logger;

import java.util.ArrayList;

public class Demo {
  private Logger logger = Logger.getInstance("Demo");
  private Object[] array;

  public <T> T[] getAsArray() {
    return (T[]) array;
  }

  private String n;

  public ArrayList<Eddy> test(int n) {
    return null;
  }

}
