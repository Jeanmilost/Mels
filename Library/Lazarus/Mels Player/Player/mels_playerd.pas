{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Mels_PlayerD;

interface

uses
    UTQRPlayerAL_Register, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UTQRPlayerAL_Register', @UTQRPlayerAL_Register.Register);
end;

initialization
  RegisterPackage('Mels_PlayerD', @Register);
end.
