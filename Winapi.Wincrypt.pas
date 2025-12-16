unit Winapi.Wincrypt;

interface

uses
  Winapi.Windows;

const
  CRYPTPROTECT_UI_FORBIDDEN = $1;

type
  PDATA_BLOB = ^DATA_BLOB;
  _CRYPTOAPI_BLOB = record
    cbData:  DWORD;
    pbData:  PByte;
  end;
  DATA_BLOB = _CRYPTOAPI_BLOB;

function CryptProtectData(
  pDataIn: PDATA_BLOB;
  szDataDescr: LPCWSTR;
  pOptionalEntropy: PDATA_BLOB;
  pvReserved:  Pointer;
  pPromptStruct:  Pointer;
  dwFlags: DWORD;
  pDataOut: PDATA_BLOB
): BOOL; stdcall;

function CryptUnprotectData(
  pDataIn: PDATA_BLOB;
  ppszDataDescr:  Pointer;
  pOptionalEntropy: PDATA_BLOB;
  pvReserved:  Pointer;
  pPromptStruct: Pointer;
  dwFlags: DWORD;
  pDataOut: PDATA_BLOB
): BOOL; stdcall;

implementation

const
  crypt32 = 'crypt32.dll';

function CryptProtectData; external crypt32 name 'CryptProtectData';
function CryptUnprotectData; external crypt32 name 'CryptUnprotectData';

end.