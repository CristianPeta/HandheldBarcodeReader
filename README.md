# HandheldBarcodeReader
Example of reading barcodes on Honeywell and Zebra handheld scanners with Android using a Delphi FMX project.

Many thanks to Brian Long (blong.com) for examples accessing Java code from Delphi and to Stéphane Wierzbicki with the help of restructuring the code.

If someone want to add barcode reading capability to own project it must look at:
1. Changes in AndroidManifest.template.xml
2. The libraries added to the project (com.symbol.emdk.jar, DataCollection.jar, honeywell.jar, NativeActivitySubclass.jar)
3. Add BarcodeReader in conditional defines
4. Code in the MainUI.pas
