{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit mels_vclcomponentsgld;

interface

uses
    UTQRVCLModelsGL_Register, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UTQRVCLModelsGL_Register', @UTQRVCLModelsGL_Register.Register);
end;

initialization
  RegisterPackage('mels_vclcomponentsgld', @Register);
end.
