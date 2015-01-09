## Setup
========

This is basically following [this](http://confluence.jetbrains.com/display/IDEADEV/Getting+Started+with+Plugin+Development#GettingStartedwithPluginDevelopment-anchor2) guide, plus some additional steps not mentioned because everybody already knows.

1. Download IntelliJ

From [here](https://www.jetbrains.com/idea/download/).

2. Get IntelliJ sources and build IntelliJ using IntelliJ

Instructions are [here](http://www.jetbrains.org/pages/viewpage.action?pageId=983225), but watch out for the notes below:

* On modern MacOS, you will have to install JDK1.6, which you can get [here](http://support.apple.com/kb/DL1572). Once you have installed this, Java 1.6 should be in /System/Library/Java/JavaVirtualMachines. If it isn't, go to developer.apple.com/downloads, search for Java, and install the Java for OS X 2013-005 package, which ought to work.
*
* You do not have to copy lib/tools.jar anywhere. Ignore this step.
* You have to build IntelliJ, but we will never actually use the version of IntelliJ you built.

3. Configure an IntelliJ IDEA SDK

Follow the "Configuring IntelliJ IDEA SDK" from the [guide](http://www.jetbrains.org/pages/viewpage.action?pageId=983225), but watch out for the notes below.

* You must use the installation directory of the downloaded, not the built version of IntelliJ (typically, /Applications/IntelliJ IDEA/Content), which fortunately should be selected by default.
* You must use JDK 1.6 (you have named this IDEA jdk earlier) as the internal Java platform.
* Make sure you build IntelliJ first (see 2.) before doing this.

4. Open the eddy plugin

Clone git@github.com:eddysystems/eddy-plugin, and open it as a directory. It should now work.

5. To see debug output using the Logger class used in the eddy plugin, make sure you see the IDEA Log in the output, and set the debug level to "all".


### Logging

We use the following Amazon services for logging:

1. DynamoDB: An "eddy-log" table with primary key "install" and range key "time".
   "install" is a cryptographic random number unique to a given installation of eddy, and
   "time" is gettimeofday (Greenwich time down to microseconds).

2. Cogito: An "eddy users" pool with a role that gives it write-only access to eddy-log.
