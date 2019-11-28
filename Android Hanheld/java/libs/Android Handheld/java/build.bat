@echo OFF

setlocal

set ANDROID_PLATFORM="C:\Users\Public\Documents\Embarcadero\Studio\19.0\CatalogRepository\AndroidSDK-2433_19.0.29899.2631\platforms\android-22"
set EMBO_LIB="C:\Program Files (x86)\Embarcadero\Studio\19.0\lib\android\debug"
set JAVA_JDK="C:\Program Files\Java\jdk1.7.0_71\bin"
set OUTPUT=output\classes com\winarhi\nativeactivitysubclass
set VERBOSE=0

echo.
echo Compiling the Java source files
echo.
mkdir output\classes 2> nul
if x%VERBOSE% == x1 SET VERBOSE_FLAG=-verbose

@echo ON
%JAVA_JDK%\javac %VERBOSE_FLAG% -source 1.7 -target 1.7 -Xlint:deprecation -cp %ANDROID_PLATFORM%\android.jar;%EMBO_LIB%\fmx.jar;libs\honeywell.jar;libs\DataCollection.jar;libs\com.symbol.emdk(stub).jar -d %OUTPUT%\NativeActivitySubclass.java
@echo OFF

echo.
echo Creating jar containing the new classes
echo.
mkdir output\jar 2> nul
if x%VERBOSE% == x1 SET VERBOSE_FLAG=v

@echo ON
%JAVA_JDK%\jar c%VERBOSE_FLAG%f output\jar\NativeActivitySubclass.jar -C output\classes com
@echo OFF

@echo ON
del "..\..\Android Handheld Demo 10.3 Rio\Android\Release\NativeActivitySubclass-dexed.jar"
del "..\..\Android Handheld Demo 10.2 Tokyo\Android\Release\NativeActivitySubclass-dexed.jar"
del "..\..\WAEagle_ClientGetMob\Android\Release\NativeActivitySubclass-dexed.jar"
@echo OFF

:Exit

endlocal
