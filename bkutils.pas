{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit bkutils;

{$warn 5023 off : no warning about unused units}
interface

uses
  Base32, ErrorMsg, Logging, rbtree, Buffers, TempFiles, alarms, passwordman, 
  streamutils, binarytree, btree, Crypto, DiffieHellman, Rand, bkutilsreg, 
  whatsmyip, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('bkutilsreg', @bkutilsreg.Register);
end;

initialization
  RegisterPackage('bkutils', @Register);
end.
