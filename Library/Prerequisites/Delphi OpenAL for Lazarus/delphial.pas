{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DelphiAL;

interface

uses
    openal, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('DelphiAL', @Register);
end.
