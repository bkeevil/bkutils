{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit bkutils;

interface

uses
  Base32, btreefile, ErrorMsg, Files, Logging, rbtree, Buffers, TempFiles, 
  streamutils, passwordman, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('bkutils', @Register);
end.
