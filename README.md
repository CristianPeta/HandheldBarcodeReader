# HandheldBarcodeReader
Example of reading barcodes on Honeywell and Zebra handheld scanners with Android using a Delphi FMX project.

Many thanks to Brian Long (blong.com) for examples accessing Java code from Delphi and to Stéphane Wierzbicki with the help of restructuring the code.

If you want to add barcode reading capability to own project you must look at:
1. Changes in AndroidManifest.template.xml
2. Adding Java libraries to the project (com.symbol.emdk.jar, DataCollection.jar, honeywell.jar, NativeActivitySubclass.jar)
3. Adding Androidapi.Handheld, Androidapi.Interop.Scanner, Androidapi.NativeActivitySubclass units to your project
4. Adding BarcodeReader in conditional defines
5. Code in the MainUI.pas
