{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit bkutils;

{$warn 5023 off : no warning about unused units}
interface

uses
  Base32, ErrorMsg, Logging, rbtree, Buffers, TempFiles, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('bkutils', @Register);
end.
