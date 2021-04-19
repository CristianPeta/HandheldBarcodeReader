# HandheldBarcodeReader
A library with examples of reading barcodes on Honeywell and Zebra handheld scanners with Android using a Delphi FMX project.

Many thanks to Brian Long (blong.com) for examples accessing Java code from Delphi and to Stéphane Wierzbicki with the help of restructuring the code.

If you want to add barcode reading capability using java libraries to your own project you must look at:
1. Changes in AndroidManifest.template.xml
2. Adding Java libraries to the project (com.symbol.emdk.jar, DataCollection.jar, honeywell.jar, NativeActivitySubclass.jar)
3. Adding Androidapi.Handheld, Androidapi.Interop.Scanner, Androidapi.Interop.Scanner.Types, Androidapi.Interop.Scanner.ZebraDW, Androidapi.NativeActivitySubclass units to your project
4. Adding BarcodeReader in conditional defines
5. Code in the MainUI.pas

If you want to use only Zebra DataWedge then you only need to use TZebraDW_BarCodeScanner from Androidapi.Interop.Scanner.ZebraDW and put into the AndroidManifest.template.xml folowing lines like in DemoZebraDW app:
          <action android:name="%package%.ZebraDW.ACTION" />
          <category android:name="android.intent.category.DEFAULT" />