eddy: autocorrect for java
==========================

eddy is a plugin for the IntelliJ Java IDE that translates pseudocode and broken Java
into correct code as you type, making its best effort to understand what you meant to
write.  eddy can be installed within IntelliJ: just 

eddy is open source, with a [BSD 2-clause license](https://github.com/eddysystems/eddy/blob/master/LICENSE).

### Install

To install eddy from within IntelliJ, go to "Preferences / Plugins" and click
"Browse Repositories". You can search for "eddy" or select it from the list.
For more details, see https://eddy.systems.

### Developer setup

Although the eddy plugin is compatible with IntelliJ 13 and up, including Android Studio,
we recommend developing eddy using IntelliJ 14 or higher.  These instructions follow
[IntelliJ's plugin guide](http://confluence.jetbrains.com/display/IDEADEV/Getting+Started+with+Plugin+Development#GettingStartedwithPluginDevelopment-anchor2).

1. Download [IntelliJ](https://www.jetbrains.com/idea/download).

2. Get IntelliJ sources and
   [build IntelliJ using IntelliJ](http://www.jetbrains.org/pages/viewpage.action?pageId=983225),
   but watch out for:

   * On modern MacOS, you will have to [install JDK1.6](http://support.apple.com/kb/DL1572).
     Once you have installed this, Java 1.6 should be in /System/Library/Java/JavaVirtualMachines.
     If it isn't, go to developer.apple.com/downloads, search for Java, and install the Java for
     OS X 2013-005 package, which ought to work.
   * You do not have to copy lib/tools.jar anywhere. Ignore this step.
   * You have to build IntelliJ, but we will never actually use the version of IntelliJ you built.

3. [Configure an IntelliJ IDEA SDK](http://www.jetbrains.org/pages/viewpage.action?pageId=983225),
   but watch out for:

   * You must use the installation directory of the downloaded, not the built version of IntelliJ
     (typically, /Applications/IntelliJ IDEA/Content), which fortunately should be selected by default.
   * You must use JDK 1.6 (you have named this IDEA jdk earlier) as the internal Java platform.
   * Make sure you build IntelliJ first (see 2.) before doing this.

4. Clone git@github.com:eddysystems/eddy, and open it in IntelliJ.

5. To see debug output using the Logger class used in the eddy plugin, make sure you see the IDEA
   Log in the output, and set the debug level to "all".

### Contributing

We welcome contributions to eddy!  If you would like to contribute, it is as easy as forking the
repository on GitHub, making your changes, and issuing a pull request.  If you have any questions,
don't hesitate to ask on the mailing list:
[eddy@googlegroups.com](https://groups.google.com/forum/#!forum/eddy).
