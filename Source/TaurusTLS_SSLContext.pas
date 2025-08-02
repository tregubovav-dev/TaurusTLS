{******************************************************************************}
{*  TaurusTLS                                                                 *}
{*           https://github.com/JPeterMugaas/TaurusTLS                        *}
{*                                                                            *}
{*  Copyright (c) 2024 - 2025 TaurusTLS Developers, All Rights Reserved       *}
{*                                                                            *}
{* Portions of this software are Copyright (c) 1993 – 2018,                   *}
{* Chad Z. Hower (Kudzu) and the Indy Pit Crew – http://www.IndyProject.org/  *}
{******************************************************************************}

{$I TaurusTLSCompilerDefines.inc}
/// <summary>
///   Declares "container" classes to operate with OpenSSL public and private entities.
/// </summary>

unit TaurusTLS_SSLContext;
{$I TaurusTLSLinkDefines.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  TaurusTLSHeaders_types, TaurusTLSHeaders_bio, TaurusTLSHeaders_evp,
  TaurusTLSExceptionHandlers, TaurusTLS_Random,
  IdGlobal, IdCTypes;

type
  ITaurusTLS_Bio = interface
  ['{FE133CBE-0D85-46A3-9F62-A4FED6EF6F3F}']
    function GetBio: PBIO;
    property Bio: PBIO read GetBio;
  end;

  ITaurusTLS_Bytes = interface
  ['{EE497C17-DE0C-4470-80B8-B0EC528B62F7}']
    function NewBio: ITaurusTLS_Bio;
    function GetBytes: TBytes;
    property Bytes: TBytes read GetBytes;
  end;

(*
  ITaurusTLS_EncryptDecryptOSSL = interface
  ['{606CD72B-AC70-4115-ADD8-23A185C93DC8}']
    function Encrypt(const APlain: TBytes; var ASecret: TBytes): TIdC_INT;
    function Decrypt(const ASecret: TBytes; var APlain: TBytes): TIdC_INT;
  end;

  ITaurusTLS_EncryptDecrypt = interface
  ['{606CD72B-AC70-4115-ADD8-23A185C93DC8}']
    procedure Encrypt(const APlain: TBytes; var ASecret: TBytes);
    procedure Decrypt(const ASecret: TBytes; var APlain: TBytes);
  end;
*)
  TTaurusTLS_CustomEncrypDecrypt = class;

  TTaurusTLS_CustomBytes = class abstract(TInterfacedObject, ITaurusTLS_Bytes)
{$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} protected type

    TBio = class(TInterfacedObject, ITaurusTLS_Bio)
  {$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} private
      FOwner: ITaurusTLS_Bytes;
      FBytes: TBytes;
      FBio: PBIO;
  {$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} protected
      function GetBio: PBIO; {$IFDEF USE_INLINE}inline;{$ENDIF}
    public
      constructor Create(ABytes: TBytes; AOwner: TTaurusTLS_CustomBytes);
      destructor Destroy; override;
    end;

{$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} protected
    function GetBytes: TBytes; virtual; abstract;
    function NewBio: ITaurusTLS_Bio; virtual; abstract;
  end;

  TTaurusTLS_PlainBytes = class(TTaurusTLS_CustomBytes)
{$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} protected
    FData: TBytes;
    function GetBytes: TBytes; override;
    function NewBio: ITaurusTLS_Bio; override;

  public
    constructor Create(const AData: TBytes); overload;
    constructor Create(ASize: TIdC_SIZET); overload;
  end;

  TTaurusTLS_WipingBytes = class(TTaurusTLS_PlainBytes)
{$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} protected
    procedure WipeData; overload;
  public
    class procedure WipeData(var AData: TBytes; AChar: Char = #0); overload;
      static; {$IFDEF USE_INLINE}inline;{$ENDIF}
    destructor Destroy; override;
  end;

  ITaurusTLS_BytesHost = interface
  ['{D4C93767-9DFB-4ADE-A8A0-4586CEB0A135}']
    procedure ReleaseNotify(const ASender: TTaurusTLS_CustomBytes);
  end;

  TTaurusTLS_EncryptedBytes = class(TTaurusTLS_WipingBytes, ITaurusTLS_BytesHost)
{$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} protected type
    TPlainBytes = class(TTaurusTLS_WipingBytes)
    {$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} private
      FHost: ITaurusTLS_BytesHost;
    public
      constructor Create(ABytes: TBytes; AHost: ITaurusTLS_BytesHost);
      destructor Destroy; override;
    end;

{$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} private
    [volatile]
    FPlainBytes: TPlainBytes;
    FEnv: TTaurusTLS_CustomEncrypDecrypt;

{$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} protected
    function NewBio: ITaurusTLS_Bio; override;
    procedure ReleaseNotify(const ASender: TTaurusTLS_CustomBytes);

    function DecryptBytes: TBytes;
    function EncrypBytes(ASrc: TBytes): TBytes;
    function GetPlainBytes: ITaurusTLS_Bytes;
  public
    constructor Create(ABytes: TBytes; AEncryptor: TTaurusTLS_CustomEncrypDecrypt);
    destructor Destroy; override;
  end;

  TTaurusTLS_Cipher = class
{$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} private
    FOwnCipher: boolean;
    FCipher: PEVP_CIPHER;
    FBlockSize: TIdC_UINT;
    FKeyLen: TIdC_UINT;
    FIVLen: TIdC_UINT;
    FBlockBits: TIdC_UINT;
    FFlags: TIdC_ULONG;
    FMode: TIdC_INT;
  protected
    class function CheckOSSLResult(AResult: TIdC_INT;
      ARaiseException: boolean): boolean; static;
    class procedure RaiseError(const AErrCode : TIdC_ULONG;
      const AMsg : String = ''); static;
  public
    constructor Create(ACipher: PEVP_CIPHER; AOwnCipher: boolean); overload;
    constructor Create(ACipherName: string); overload;
    constructor Create(ACipherName: PIdAnsiChar); overload;
    destructor Destroy; override;

    class function GetCipherByName(ACipherName: string): PEVP_CIPHER;
      overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetCipherByName(ACipherName: PIdAnsiChar): PEVP_CIPHER;
      overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}

    function NewKey: TBytes; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function NewIV: TBytes; {$IFDEF USE_INLINE}inline;{$ENDIF}

    property Cipher: PEVP_CIPHER read FCipher;
    property KeyLen: TIdC_UINT read FKeyLen;
    property IVLen: TIdC_UINT read FIVLen;
    property BlockSize: TIdC_UINT read FBlockSize;
    property BlockBits: TIdC_UINT read FBlockBits;
    property Flags: TIdC_ULONG read FFlags;
    property Mode: TIdC_INT read FMode;
  end;

  TTaurusTLS_CustomEncrypDecryptClass = class of TTaurusTLS_CustomEncrypDecrypt;
  TTaurusTLS_CustomEncrypDecrypt = class abstract
//  TTaurusTLS_EncryptDecrypt = class(TInterfacedObject, ITaurusTLS_EncryptDecrypt)
  strict private class var
    FDefaultCipher: PEVP_CIPHER;

{$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} private
    FCipher: TTaurusTLS_Cipher;
    FCtx: PEVP_CIPHER_CTX;
    FKey: TBytes;
    FIV: TBytes;

    class function CheckOSSLResult(AResult: TIdC_INT;
      ARaiseException: boolean): boolean; static;
    class procedure RaiseError(const AErrCode : TIdC_ULONG;
      const AMsg : String = ''); static;
    function GetBlockBits: TIdC_UINT;
    function GetBlockSize: TIdC_UINT;
    function GetFlags: TIdC_ULONG;
    function GetIVLen: TIdC_UINT;
    function GetKeyLen: TIdC_UINT;
    function GetMode: TIdC_INT;

{$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} protected
    function NewContext: PEVP_CIPHER_CTX;
    function GetAdjustedSize(ASize: TIdC_SIZET): TIdC_SIZET;
      {$IFDEF USE_INLINE}inline;{$ENDIF}

    procedure DefaultDecrypt(const ASecret: TBytes; var APlain: TBytes);
    procedure DefaultEncrypt(const APlain: TBytes; var ASecret: TBytes);

    class function NewDefaultCipher: PEVP_CIPHER; virtual; abstract;
    class procedure ReleaseCipher(ACipher: PEVP_CIPHER); static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure SetDefaultCipher(ACipher: PEVP_CIPHER); static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetDefaultCipher: PEVP_CIPHER; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetDefaultCipherName: string; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure SetDefaultCipherName(ACipherName: string); overload; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure SetDefaultCipherName(ACipherName: PIdAnsiChar); overload; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CipherInstanceFromDefault: TTaurusTLS_Cipher; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}

    class property DefaultCipher: PEVP_CIPHER read GetDefaultCipher write SetDefaultCipher;
    class property DefaultCipherName: string read GetDefaultCipherName
      write SetDefaultCipherName;

    property Ctx: PEVP_CIPHER_CTX read FCtx;

    constructor Create(ACipher: TTaurusTLS_Cipher); overload;
  public
    class destructor Destroy;
    constructor Create; overload;
    destructor Destroy; override;
    procedure Encrypt(const APlain: TBytes; var ASecret: TBytes); virtual; abstract;
    procedure Decrypt(const ASecret: TBytes; var APlain: TBytes); virtual; abstract;

    property KeyLen: TIdC_UINT read GetKeyLen;
    property IVLen: TIdC_UINT read GetIVLen;
    property BlockSize: TIdC_UINT read GetBlockSize;
    property BlockBits: TIdC_UINT read GetBlockBits;
    property Flags: TIdC_ULONG read GetFlags;
    property Mode: TIdC_INT read GetMode;

  end;

  TTaurusTLS_SimpleAESEncrypDecrypt = class(TTaurusTLS_CustomEncrypDecrypt)
  public type
    TAESKeySize = (aks128, aks192, aks256);
    TEncoderMode = (emCBC=EVP_CIPH_CBC_MODE, emCFB=EVP_CIPH_CFB_MODE,
                    emOFB=EVP_CIPH_OFB_MODE, emCTR=EVP_CIPH_CTR_MODE);
  public const
    cDefaultCipherName = 'AES-192-CTR';
  protected const
    cKeySizes: array[TAESKeySize] of string = ('128','192','256');
    cEncoderModes: array[TEncoderMode] of string = ('CBC','CFB','OFB','CTR');
{$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} protected
    class function NewDefaultCipher: PEVP_CIPHER; override;
  public
    constructor Create(AKeySize: TAESKeySize; AEncoderMode: TEncoderMode);
    procedure Encrypt(const APlain: TBytes; var ASecret: TBytes); override;
    procedure Decrypt(const ASecret: TBytes; var APlain: TBytes); override;

    class function BuildCipherName(AKeySize: TAESKeySize;
      AEncoderMode: TEncoderMode): string; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
  end;

  EETaurusTLSAPICipherError = class(ETaurusTLSAPICryptoError);
  EETaurusTLSAPIEncrypDecryptError = class(ETaurusTLSAPICryptoError);

(* --- To transfer consts to the TaurusTLSHeaders_evperr.pas --- *)
const
  EVP_R_INVALID_IV_LENGTH = 194;

(* --- To transfer resourcestrings to the TaurusTLS_ResourceStrings.pas --- *)
resourcestring
  REVP_Cipher_NoCipherProvided = 'Unable to initializate Cipher instance. '+
    'No OpenSSL Cipher provided.';
  REVP_Cipher_ZeroKeyLen = 'Unable to initializate Cipher instance with zero Key length.';
  REVP_Cipher_IVKeyLen = 'Unable to initializate Cipher instance with zero IV length.';
  REVP_EncryptDecrypt_InitNoCipher = 'Unable to set ''nil'' value to the Default Cipher';


implementation

uses
  TaurusTLSLoader,
  TaurusTLSHeaders_evperr, TaurusTLS_ResourceStrings;

{ TTaurusTLS_CustomBytes.TBioIntf }

constructor TTaurusTLS_CustomBytes.TBio.Create(ABytes: TBytes;
  AOwner: TTaurusTLS_CustomBytes);
begin
  FOwner:=AOwner;
  FBytes:=ABytes;
end;

destructor TTaurusTLS_CustomBytes.TBio.Destroy;
begin
  if Assigned(FBio) then
    BIO_free(FBio);
  inherited;
end;

function TTaurusTLS_CustomBytes.TBio.GetBio: PBIO;
begin
  if not Assigned(FBio) and (Length(FBytes) > 0) then
    FBIO:=BIO_new_mem_buf(FBytes[0], Length(FBytes));
  Result:=FBIO;
end;

{ TTaurusTLS_PlainBytes }

constructor TTaurusTLS_PlainBytes.Create(const AData: TBytes);
begin
  inherited Create;
  FData:=AData;
end;

constructor TTaurusTLS_PlainBytes.Create(ASize: TIdC_SIZET);
var
  lData: TBytes;

begin
  SetLength(lData, ASize);
  Create(lData);
end;

function TTaurusTLS_PlainBytes.GetBytes: TBytes;
begin
  Result:=FData;
end;

function TTaurusTLS_PlainBytes.NewBio: ITaurusTLS_Bio;
begin
  if Length(FData) > 0 then
    Result:=TBio.Create(FData, Self)
  else
    Result:=nil;
end;

{ TTaurusTLS_WipedBytes }

destructor TTaurusTLS_WipingBytes.Destroy;
begin
  WipeData;
  inherited;
end;

procedure TTaurusTLS_WipingBytes.WipeData;
begin
  WipeData(FData);
end;

class procedure TTaurusTLS_WipingBytes.WipeData(var AData: TBytes; AChar: Char);
var
  lLen: NativeUInt;

begin
  lLen:=Length(AData);
  if lLen = 0 then
    Exit;
  FillChar(AData[0], lLen, AChar);
  SetLength(AData, 0);
end;

{ TTaurusTLS_EncryptedBytes.TPlainBytes }

constructor TTaurusTLS_EncryptedBytes.TPlainBytes.Create(ABytes: TBytes; AHost: ITaurusTLS_BytesHost);
begin
  // Assert is added intentionally to avoid this class improper usage.
  Assert(Assigned(AHost),
    ClassName+'.Create: parameter AHost must not be  ''nil''.');
  FHost:=AHost;
  inherited Create(Abytes);
end;

destructor TTaurusTLS_EncryptedBytes.TPlainBytes.Destroy;
begin
  FHost.ReleaseNotify(Self);
  inherited;
end;

{ TTaurusTLS_EncryptedBytes }

constructor TTaurusTLS_EncryptedBytes.Create(ABytes: TBytes;
  AEncryptor: TTaurusTLS_CustomEncrypDecrypt);
begin
  // Assert is added intentionally to avoid this class improper usage.
  Assert(Assigned(AEncryptor),
    ClassName+'.Create: parameter AEncryptor must not be ''nil''.');
  FEnv:=AEncryptor;
  inherited Create(EncrypBytes(ABytes));
end;

function TTaurusTLS_EncryptedBytes.DecryptBytes: TBytes;
begin
  FEnv.Decrypt(FData, Result);
end;

destructor TTaurusTLS_EncryptedBytes.Destroy;
begin
  FreeAndNil(FEnv);
  inherited;
end;

function TTaurusTLS_EncryptedBytes.EncrypBytes(ASrc: TBytes): TBytes;
begin
  FEnv.Encrypt(ASrc, Result);
end;

function TTaurusTLS_EncryptedBytes.GetPlainBytes: ITaurusTLS_Bytes;
var
  lOldPlainBytes, lNewPlainBytes: TPlainBytes;
  lNewPlainData: TBytes;

begin
  Result:=FPlainBytes;
  lOldPlainBytes:=FPlainBytes;
  if not Assigned(lOldPlainBytes) then
  begin
    lNewPlainData:=DecryptBytes;
    lNewPlainBytes:=TPlainBytes.Create(lNewPlainData, Self);
    if TInterlocked.CompareExchange(Pointer(FPlainBytes), Pointer(lNewPlainData),
    Pointer(lOldPlainBytes)) <> nil then
      lNewPlainBytes.Free
    else
      Result:=lNewPlainBytes;
  end;
end;

function TTaurusTLS_EncryptedBytes.NewBio: ITaurusTLS_Bio;
begin
  Result:=GetPlainBytes.NewBio;
end;

procedure TTaurusTLS_EncryptedBytes.ReleaseNotify(const ASender: TTaurusTLS_CustomBytes);
{$IFDEF DEBUG}
var
  lOldBytes: TTaurusTLS_CustomBytes;
{$ENDIF}
begin
  if ASender <> FPlainBytes then
    Exit;
{$IFDEF DEBUG}
  lOldBytes:=
{$ENDIF}
  TInterlocked.CompareExchange(Pointer(FPlainBytes), nil,
    Pointer(ASender));
end;

{ TOSSLCipher }

constructor TTaurusTLS_Cipher.Create(ACipher: PEVP_CIPHER; AOwnCipher: boolean);

  {$R-}{$Q-}
  function BitsLen(ABytes: NativeUInt): byte; {$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    Result:=0;
    while ABytes > 1 do
    begin
      Inc(Result);
      ABytes:=ABytes shr 1;
    end;
  end;
  {$Q+}{$R+}

begin
  inherited Create;
  if not Assigned(ACipher) then
    RaiseError(EVP_R_NO_CIPHER_SET, REVP_Cipher_NoCipherProvided);
  FCipher:=ACipher;
  FOwnCipher:=AOwnCipher;
  FKeyLen:=EVP_CIPHER_get_key_length(ACipher);
  if FKeyLen = 0 then
    FKeyLen:=EVP_MAX_KEY_LENGTH;
//    RaiseError(EVP_R_INVALID_KEY_LENGTH, REVP_Cipher_ZeroKeyLen);
  FIVLen:=EVP_CIPHER_get_iv_length(ACipher);
  if FIVLen = 0 then
    FIVLen:=EVP_MAX_IV_LENGTH;
//    RaiseError(EVP_R_INVALID_IV_LENGTH, REVP_Cipher_IVKeyLen);
  FBlockSize:=EVP_CIPHER_get_block_size(ACipher);
  FBlockBits:=BitsLen(FBlockSize);
  FFlags:=EVP_CIPHER_get_flags(ACipher);
  FMode:=EVP_CIPHER_get_mode(ACipher);
end;

constructor TTaurusTLS_Cipher.Create(ACipherName: string);
begin
  Create(GetCipherByName(ACipherName), True);
end;

constructor TTaurusTLS_Cipher.Create(ACipherName: PIdAnsiChar);
begin
  Create(GetCipherByName(ACipherName), True);
end;

destructor TTaurusTLS_Cipher.Destroy;
begin
  if FOwnCipher and Assigned(FCipher) then
    EVP_CIPHER_free(FCipher);
  inherited;
end;

class function TTaurusTLS_Cipher.GetCipherByName(
  ACipherName: PIdAnsiChar): PEVP_CIPHER;
begin
  Result:=EVP_Cipher_fetch(nil, ACipherName, nil);
end;

class function TTaurusTLS_Cipher.GetCipherByName(
  ACipherName: string): PEVP_CIPHER;
var
  lAnsiStr: RawByteString;

begin
{$WARNINGS OFF}
  lAnsiStr:=ACipherName; //expicit conversion from Unicode to Ansi string
{$WARNINGS ON}
  Result:=GetCipherByName(PIdAnsiChar(lAnsiStr));
end;

class function TTaurusTLS_Cipher.CheckOSSLResult(AResult: TIdC_INT;
  ARaiseException: boolean): boolean;
begin
  Result:=AResult = 1;
  if (not Result) and ARaiseException then
    EETaurusTLSAPICipherError.RaiseException;
end;

class procedure TTaurusTLS_Cipher.RaiseError(const AErrCode: TIdC_ULONG;
  const AMsg: String);
begin
  ETaurusTLSAPICryptoError.RaiseExceptionCode(AErrCode, AMsg);
end;

function TTaurusTLS_Cipher.NewIV: TBytes;
begin
  Result:=TTaurusTLS_Random.PublicRandom.Random(IVLen);
end;

function TTaurusTLS_Cipher.NewKey: TBytes;
begin
  Result:=TTaurusTLS_Random.PrivateRandom.Random(KeyLen);
end;

{ TTaurusTLS_EncryptDecrypt }

class destructor TTaurusTLS_CustomEncrypDecrypt.Destroy;
begin
//  ReleaseCipher(FDefaultCipher);
end;

constructor TTaurusTLS_CustomEncrypDecrypt.Create(ACipher: TTaurusTLS_Cipher);
begin
  inherited Create;
  if not Assigned(ACipher) then
    ACipher:=CipherInstanceFromDefault;
  FCipher:=ACipher;
  FKey:=ACipher.NewKey;
  FIV:=ACipher.NewIV;
  FCtx:=NewContext;
end;

constructor TTaurusTLS_CustomEncrypDecrypt.Create;
begin
  Create(nil);
end;

destructor TTaurusTLS_CustomEncrypDecrypt.Destroy;
begin
  FreeAndNil(FCipher);
  inherited;
end;

class function TTaurusTLS_CustomEncrypDecrypt.CipherInstanceFromDefault: TTaurusTLS_Cipher;
begin
  Result:=TTaurusTLS_Cipher.Create(DefaultCipher, False);
end;

class function TTaurusTLS_CustomEncrypDecrypt.CheckOSSLResult(AResult: TIdC_INT;
  ARaiseException: boolean): boolean;
begin
  Result:=AResult = 1;
  if (not Result) and ARaiseException then
    EETaurusTLSAPIEncrypDecryptError.RaiseException;
end;

class procedure TTaurusTLS_CustomEncrypDecrypt.RaiseError(const AErrCode: TIdC_ULONG;
  const AMsg: String);
begin
  EETaurusTLSAPIEncrypDecryptError.RaiseExceptionCode(AErrCode, AMsg);
end;

procedure TTaurusTLS_CustomEncrypDecrypt.DefaultEncrypt(const APlain: TBytes;
  var ASecret: TBytes);
var
  lLen, lResultLen: TIdC_INT;
  lCipher: PEVP_CIPHER;
  lCtx: PEVP_CIPHER_CTX;

begin
  lLen:=Length(APlain);
  if lLen = 0 then
  begin
    SetLength(ASecret, 0);
    Exit;
  end;
  try
    lCtx := Ctx;
    lCipher := FCipher.Cipher;
    SetLength(ASecret, GetAdjustedSize(lLen));
    CheckOSSLResult(EVP_CipherInit_ex(lCtx, lCipher, nil, @FKey[0], @FIV[0],
      Ord(True)), True);
    CheckOSSLResult(EVP_CipherUpdate(lCtx, ASecret[0], lResultLen, APlain[0],
      lLen), True);
    CheckOSSLResult(EVP_CipherFinal_ex(lCtx, ASecret[lResultLen], lLen), True);
    SetLength(ASecret, lResultLen + lLen);
  except
    SetLength(ASecret, 0);
    raise;
  end;
end;

procedure TTaurusTLS_CustomEncrypDecrypt.DefaultDecrypt(const ASecret: TBytes;
  var APlain: TBytes);
var
  lLen, lResultLen: TIdC_INT;
  lCipher: PEVP_CIPHER;
  lCtx: PEVP_CIPHER_CTX;

begin
  lLen:=Length(ASecret);
  if lLen = 0 then
  begin
    SetLength(APlain, 0);
    Exit;
  end;
  try
    lCtx := Ctx;
    lCipher := FCipher.Cipher;
    SetLength(APlain, GetAdjustedSize(lLen));
    CheckOSSLResult(EVP_CipherInit_ex(lCtx, lCipher, nil, @FKey[0], @FIV[0],
      Ord(False)), True);
    CheckOSSLResult(EVP_CipherUpdate(lCtx, APlain[0], lResultLen, ASecret[0],
      lLen), True);
    CheckOSSLResult(EVP_CipherFinal_ex(lCtx, APlain[lResultLen], lLen), True);
    SetLength(APlain, lResultLen + lLen);
  except
    SetLength(APlain, 0);
    raise;
  end;
end;

function TTaurusTLS_CustomEncrypDecrypt.GetAdjustedSize(
  ASize: TIdC_SIZET): TIdC_SIZET;
var
  lBlockSize: TIdC_UINT;
  lBits: TIdC_UINT;

begin
  lBlockSize:=FCipher.BlockSize;
  lBits:=FCipher.BlockBits;
  Result:=(((ASize+lBlockSize) shr lBits) shl lBits)+lBlockSize;
end;

class function TTaurusTLS_CustomEncrypDecrypt.GetDefaultCipher: PEVP_CIPHER;
begin
  if not Assigned(FDefaultCipher) then
    SetDefaultCipher(NewDefaultCipher);
  Result:=FDefaultCipher;
end;

class procedure TTaurusTLS_CustomEncrypDecrypt.SetDefaultCipher(
  ACipher: PEVP_CIPHER);
var
  lOldCipher: PEVP_CIPHER;

begin
  if not Assigned(ACipher) then
    RaiseError(EVP_R_NO_CIPHER_SET, REVP_EncryptDecrypt_InitNoCipher);
  lOldCipher:=TInterlocked.Exchange(Pointer(FDefaultCipher), Pointer(ACipher));
  if Assigned(lOldCipher) then
    ReleaseCipher(lOldCipher);
end;

class function TTaurusTLS_CustomEncrypDecrypt.GetDefaultCipherName: string;
var
  lDefaultCipherName: PIdAnsiChar;

begin
  if Assigned(FDefaultCipher) then
  begin
{$WARNINGS OFF}
    lDefaultCipherName:=EVP_CIPHER_get0_name(FDefaultCipher);
    Result:=lDefaultCipherName;
{$WARNINGS ON}
  end
  else
    Result:='';
end;

function TTaurusTLS_CustomEncrypDecrypt.GetBlockBits: TIdC_UINT;
begin
  Result:=FCipher.BlockBits;
end;

function TTaurusTLS_CustomEncrypDecrypt.GetBlockSize: TIdC_UINT;
begin
  Result:=FCipher.BlockSize;
end;

function TTaurusTLS_CustomEncrypDecrypt.GetFlags: TIdC_ULONG;
begin
  Result:=FCipher.Flags;
end;

function TTaurusTLS_CustomEncrypDecrypt.GetIVLen: TIdC_UINT;
begin
  Result:=FCipher.IVLen;
end;

function TTaurusTLS_CustomEncrypDecrypt.GetKeyLen: TIdC_UINT;
begin
  Result:=FCipher.KeyLen;
end;

function TTaurusTLS_CustomEncrypDecrypt.GetMode: TIdC_INT;
begin
  Result:=FCipher.Mode;
end;

function TTaurusTLS_CustomEncrypDecrypt.NewContext: PEVP_CIPHER_CTX;
begin
  Result:=EVP_CIPHER_CTX_new;
  if not Assigned(Result) then
    CheckOSSLResult(0, True);
end;

class procedure TTaurusTLS_CustomEncrypDecrypt.ReleaseCipher(ACipher: PEVP_CIPHER);
begin
{$IFNDEF OPENSSL_STATIC_LINK_MODEL}
  if Assigned(@EVP_CIPHER_free) then
    EVP_CIPHER_free(ACipher);
{$ELSE}
  EVP_CIPHER_free(ACipher);
{$ENDIF}
end;

class procedure TTaurusTLS_CustomEncrypDecrypt.SetDefaultCipherName(
  ACipherName: PIdAnsiChar);
begin
  SetDefaultCipher(TTaurusTLS_Cipher.GetCipherByName(ACipherName));
end;

class procedure TTaurusTLS_CustomEncrypDecrypt.SetDefaultCipherName(
  ACipherName: string);
begin
  SetDefaultCipher(TTaurusTLS_Cipher.GetCipherByName(ACipherName));
end;

{ TTaurusTLS_SimpleAESEncrypDecrypt }

constructor TTaurusTLS_SimpleAESEncrypDecrypt.Create(AKeySize: TAESKeySize;
  AEncoderMode: TEncoderMode);
begin
  inherited Create(TTaurusTLS_Cipher.Create(
    BuildCipherName(AKeySize, AEncoderMode)));
end;

class function TTaurusTLS_SimpleAESEncrypDecrypt.NewDefaultCipher: PEVP_CIPHER;
begin
  Result:=TTaurusTLS_Cipher.GetCipherByName(cDefaultCipherName);
end;

procedure TTaurusTLS_SimpleAESEncrypDecrypt.Encrypt(const APlain: TBytes;
  var ASecret: TBytes);
begin
  DefaultEncrypt(APlain, ASecret);
end;

procedure TTaurusTLS_SimpleAESEncrypDecrypt.Decrypt(const ASecret: TBytes;
  var APlain: TBytes);
begin
  DefaultDecrypt(ASecret, APlain);
end;

class function TTaurusTLS_SimpleAESEncrypDecrypt.BuildCipherName(
  AKeySize: TAESKeySize; AEncoderMode: TEncoderMode): string;
begin
  Result:=Format('AES-%s-%s',
    [cKeySizes[AKeySize], cEncoderModes[AEncoderMode]]);
end;

end.
