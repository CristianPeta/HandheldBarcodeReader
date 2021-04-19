unit Androidapi.Interop.Scanner.Types;

interface

type
  TOnScannerCompleted = procedure(ScanFormat, ScanContent: string) of object;
  TOnScannerStatus = procedure(AStatus: String) of object;

implementation

end.
