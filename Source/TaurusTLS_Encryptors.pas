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
///   Declares classes to Encrypt/Decrypt data using OpenSSL encryption.
/// </summary>

unit TaurusTLS_Encryptors;
{$I TaurusTLSLinkDefines.inc}

interface

uses
  SysUtils, TaurusTLSHeaders_types,
  TaurusTLSHeaders_evp, TaurusTLSExceptionHandlers, TaurusTLS_Random,
  IdGlobal, IdCTypes;

type
  ETaurusTLSCipherError = class(ETaurusTLSAPICryptoError);
  ETaurusTLSEncryptorError = class(ETaurusTLSAPICryptoError);

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

  TTaurusTLS_CustomEncryptorClass = class of TTaurusTLS_CustomEncryptor;
  TTaurusTLS_CustomEncryptor = class abstract
{$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} private
    FCipher: TTaurusTLS_Cipher;
    FCtx: PEVP_CIPHER_CTX;
    FKey: TBytes;
    FIV: TBytes;

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

    class procedure ReleaseCipher(ACipher: PEVP_CIPHER); static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Ctx: PEVP_CIPHER_CTX read FCtx;

    constructor Create(ACipher: TTaurusTLS_Cipher); overload;
  public
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

  TTaurusTLS_AESKeySize = (aks128, aks192, aks256);
  TTaurusTLS_EncodeModeWide = (emStream=EVP_CIPH_STREAM_CIPHER, emECB=EVP_CIPH_ECB_MODE,
    emCBC=EVP_CIPH_CBC_MODE, emCFB=EVP_CIPH_CFB_MODE, emOFB=EVP_CIPH_OFB_MODE,
    emCTR=EVP_CIPH_CTR_MODE, emGCM=EVP_CIPH_GCM_MODE, emCCM=EVP_CIPH_CCM_MODE);
  TTaurusTLS_EncodeMode = emCBC..emCTR;

  TTaurusTLS_SimpleAESEncryptor = class(TTaurusTLS_CustomEncryptor)
    class function BuildCipherName(AKeySize: TTaurusTLS_AESKeySize;
      AEncoderMode: TTaurusTLS_EncodeMode): string; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    constructor Create(AKeySize: TTaurusTLS_AESKeySize;
      AEncoderMode: TTaurusTLS_EncodeMode);
    procedure Encrypt(const APlain: TBytes; var ASecret: TBytes); override;
    procedure Decrypt(const ASecret: TBytes; var APlain: TBytes); override;
  end;

  TTaurusTLS_SimpleAESFactory = class
{$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} private class var
    FFactory: TTaurusTLS_SimpleAESFactory;
{$IFDEF DCC}
    FLock: TMultiReadExclusiveWriteSynchronizer;
{$ELSE}
    FLock: TIdCriticalSection;
{$ENDIF}
{$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} private
    FKeySize: TTaurusTLS_AESKeySize;
    FEncodeMode: TTaurusTLS_EncodeMode;
    class function GetEncoderMode: TTaurusTLS_EncodeMode; static;
    class function GetKeySize: TTaurusTLS_AESKeySize; static;
    class function GetEncodingModeName(AEncoderMode: TTaurusTLS_EncodeMode): string;
      static; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetKeySizeName(AKeySize: TTaurusTLS_AESKeySize): string;
      static; {$IFDEF USE_INLINE}inline;{$ENDIF}
  protected const
    // DO NOT LOCALIZE
    cKeySizes: array[TTaurusTLS_AESKeySize] of string = ('128','192','256');
    cEncoderModes: array[TTaurusTLS_EncodeMode] of string =
      ('CBC','CFB','OFB','CTR');
    cDefaultKey = aks192;
    cDefaultEncoder = emCFB;
  protected
    class function BuildCipherName(AKeySize: TTaurusTLS_AESKeySize;
      AEncoderMode: TTaurusTLS_EncodeMode): string; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure BeginRead; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure BeginWrite; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure EndRead; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure EndWrite; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetFactory: TTaurusTLS_SimpleAESFactory; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    class property Factory: TTaurusTLS_SimpleAESFactory read GetFactory;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure SetDefaultCipher(AKeySize: TTaurusTLS_AESKeySize;
      AEncoderMode: TTaurusTLS_EncodeMode);
    constructor Create(AKeySize: TTaurusTLS_AESKeySize;
      AEncoderMode: TTaurusTLS_EncodeMode);
    class function NewEncryptor: TTaurusTLS_CustomEncryptor; overload;
      static; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function NewEncryptor(AKeySize: TTaurusTLS_AESKeySize;
      AEncoderMode: TTaurusTLS_EncodeMode): TTaurusTLS_CustomEncryptor;
      overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class property DefaultKeySize: TTaurusTLS_AESKeySize read GetKeySize;
    class property KeySizeName[AKeySize: TTaurusTLS_AESKeySize]: string
      read GetKeySizeName;
    class property DefaultEncodeMode: TTaurusTLS_EncodeMode read GetEncoderMode;
    class property EncodeModeName[AEncoderMode: TTaurusTLS_EncodeMode]: string
      read GetEncodingModeName;
  end;

implementation

uses
  TaurusTLSHeaders_evperr, TaurusTLS_ResourceStrings;

function OsslSucceeded(AResult: TIdC_INT): boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result:=AResult = 1;
end;

function OsslFailed(AResult: TIdC_INT): boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result:=AResult <> 1;
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
     ETaurusTLSCipherError.RaiseWithMessage(REVP_Cipher_NoCipherProvided);
  FCipher:=ACipher;
  FOwnCipher:=AOwnCipher;
  FKeyLen:=EVP_CIPHER_get_key_length(ACipher);
  if FKeyLen = 0 then
    FKeyLen:=EVP_MAX_KEY_LENGTH;
  FIVLen:=EVP_CIPHER_get_iv_length(ACipher);
  if FIVLen = 0 then
    FIVLen:=EVP_MAX_IV_LENGTH;
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

function TTaurusTLS_Cipher.NewIV: TBytes;
begin
  Result:=TTaurusTLS_Random.PublicRandom.Random(IVLen);
end;

function TTaurusTLS_Cipher.NewKey: TBytes;
begin
  Result:=TTaurusTLS_Random.PrivateRandom.Random(KeyLen);
end;

{ TTaurusTLS_CustomEncryptor }

constructor TTaurusTLS_CustomEncryptor.Create(ACipher: TTaurusTLS_Cipher);
begin
  inherited Create;
  if not Assigned(ACipher) then
    raise ETaurusTLSEncryptorError.Create(REVP_EncryptDecrypt_InitNoCipher);
  FCipher:=ACipher;
  FKey:=ACipher.NewKey;
  FIV:=ACipher.NewIV;
  FCtx:=NewContext;
end;

constructor TTaurusTLS_CustomEncryptor.Create;
begin
  Create(nil);
end;

destructor TTaurusTLS_CustomEncryptor.Destroy;
begin
  FreeAndNil(FCipher);
  inherited;
end;

procedure TTaurusTLS_CustomEncryptor.DefaultEncrypt(const APlain: TBytes;
  var ASecret: TBytes);
var
  lLen, lResultLen: TIdC_INT;
  lCipher: PEVP_CIPHER;
  lCtx: PEVP_CIPHER_CTX;

  procedure RaiseException;
  begin
    ETaurusTLSEncryptorError.RaiseException(REVP_Encryptor_Encrypt_Error);
  end;

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
    if OsslFailed(EVP_CipherInit_ex(lCtx, lCipher, nil, @FKey[0], @FIV[0],
      Ord(True))) then
      RaiseException;
    if OsslFailed(EVP_CipherUpdate(lCtx, ASecret[0], lResultLen, APlain[0],
      lLen)) then
      RaiseException;
    if OsslFailed(EVP_CipherFinal_ex(lCtx, ASecret[lResultLen], lLen)) then
      RaiseException;
    SetLength(ASecret, lResultLen + lLen);
  except
    SetLength(ASecret, 0);
    raise;
  end;
end;

procedure TTaurusTLS_CustomEncryptor.DefaultDecrypt(const ASecret: TBytes;
  var APlain: TBytes);
var
  lLen, lResultLen: TIdC_INT;
  lCipher: PEVP_CIPHER;
  lCtx: PEVP_CIPHER_CTX;

  procedure RaiseException;
  begin
    ETaurusTLSEncryptorError.RaiseException(REVP_Encryptor_Decrypt_Error);
  end;

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
    if OsslFailed(EVP_CipherInit_ex(lCtx, lCipher, nil, @FKey[0], @FIV[0],
      Ord(False))) then
      RaiseException;
    if OsslFailed(EVP_CipherUpdate(lCtx, APlain[0], lResultLen, ASecret[0],
      lLen)) then
      RaiseException;
    if OsslFailed(EVP_CipherFinal_ex(lCtx, APlain[lResultLen], lLen)) then
      RaiseException;
    SetLength(APlain, lResultLen + lLen);
  except
    SetLength(APlain, 0);
    raise;
  end;
end;

function TTaurusTLS_CustomEncryptor.GetAdjustedSize(
  ASize: TIdC_SIZET): TIdC_SIZET;
var
  lBlockSize: TIdC_UINT;
  lBits: TIdC_UINT;

begin
  lBlockSize:=FCipher.BlockSize;
  lBits:=FCipher.BlockBits;
  Result:=(((ASize+lBlockSize) shr lBits) shl lBits)+lBlockSize;
end;

function TTaurusTLS_CustomEncryptor.GetBlockBits: TIdC_UINT;
begin
  Result:=FCipher.BlockBits;
end;

function TTaurusTLS_CustomEncryptor.GetBlockSize: TIdC_UINT;
begin
  Result:=FCipher.BlockSize;
end;

function TTaurusTLS_CustomEncryptor.GetFlags: TIdC_ULONG;
begin
  Result:=FCipher.Flags;
end;

function TTaurusTLS_CustomEncryptor.GetIVLen: TIdC_UINT;
begin
  Result:=FCipher.IVLen;
end;

function TTaurusTLS_CustomEncryptor.GetKeyLen: TIdC_UINT;
begin
  Result:=FCipher.KeyLen;
end;

function TTaurusTLS_CustomEncryptor.GetMode: TIdC_INT;
begin
  Result:=FCipher.Mode;
end;

function TTaurusTLS_CustomEncryptor.NewContext: PEVP_CIPHER_CTX;
begin
  Result:=EVP_CIPHER_CTX_new;
  if not Assigned(Result) then
    ETaurusTLSEncryptorError.RaiseException(REVP_Encryptor_CtxInitError);
end;

class procedure TTaurusTLS_CustomEncryptor.ReleaseCipher(ACipher: PEVP_CIPHER);
begin
{$IFNDEF OPENSSL_STATIC_LINK_MODEL}
  if Assigned(@EVP_CIPHER_free) then
    EVP_CIPHER_free(ACipher);
{$ELSE}
  EVP_CIPHER_free(ACipher);
{$ENDIF}
end;

{ TTaurusTLS_SimpleAESEncryptor }

constructor TTaurusTLS_SimpleAESEncryptor.Create(AKeySize: TTaurusTLS_AESKeySize;
  AEncoderMode: TTaurusTLS_EncodeMode);
begin
  inherited Create(TTaurusTLS_Cipher.Create(
    BuildCipherName(AKeySize, AEncoderMode)));
end;

procedure TTaurusTLS_SimpleAESEncryptor.Encrypt(const APlain: TBytes;
  var ASecret: TBytes);
begin
  DefaultEncrypt(APlain, ASecret);
end;

procedure TTaurusTLS_SimpleAESEncryptor.Decrypt(const ASecret: TBytes;
  var APlain: TBytes);
begin
  DefaultDecrypt(ASecret, APlain);
end;

class function TTaurusTLS_SimpleAESEncryptor.BuildCipherName(
  AKeySize: TTaurusTLS_AESKeySize; AEncoderMode: TTaurusTLS_EncodeMode): string;
begin
  Result:=TTaurusTLS_SimpleAESFactory.BuildCipherName(AKeySize, AEncoderMode);
end;

{ TTaurusTLS_SimpleAESFactory }

class constructor TTaurusTLS_SimpleAESFactory.Create;
begin
{$IFDEF DCC}
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
{$ELSE}
  FLock:=TIdCriticalSection.Create;
{$ENDIF}
end;

constructor TTaurusTLS_SimpleAESFactory.Create(AKeySize: TTaurusTLS_AESKeySize;
  AEncoderMode: TTaurusTLS_EncodeMode);
begin
  FKeySize:=AKeySize;
  FEncodeMode:=AEncoderMode;
end;

class destructor TTaurusTLS_SimpleAESFactory.Destroy;
begin
  FreeAndNil(FFactory);
  FreeAndNil(FLock);
end;

class function TTaurusTLS_SimpleAESFactory.BuildCipherName(
  AKeySize: TTaurusTLS_AESKeySize;
  AEncoderMode: TTaurusTLS_EncodeMode): string;
begin
  Result:=Format('AES-%s-%s',
    [cKeySizes[AKeySize], cEncoderModes[AEncoderMode]]);
end;

class procedure TTaurusTLS_SimpleAESFactory.BeginRead;
begin
{$IFDEF DCC}
  FLock.BeginRead;
{$ELSE}
  FLock.Enter;
{$ENDIF}
end;

class procedure TTaurusTLS_SimpleAESFactory.BeginWrite;
begin
{$IFDEF DCC}
  FLock.BeginWrite;
{$ELSE}
  FLock.Enter;
{$ENDIF}
end;

class procedure TTaurusTLS_SimpleAESFactory.EndRead;
begin
{$IFDEF DCC}
  FLock.EndRead;
{$ELSE}
  FLock.Leave;
{$ENDIF}
end;

class procedure TTaurusTLS_SimpleAESFactory.EndWrite;
begin
{$IFDEF DCC}
  FLock.EndWrite;
{$ELSE}
  FLock.Leave;
{$ENDIF}
end;

class function TTaurusTLS_SimpleAESFactory.GetFactory: TTaurusTLS_SimpleAESFactory;
begin
  BeginRead;
  try
    if not Assigned(FFactory) then
      SetDefaultCipher(cDefaultKey, cDefaultEncoder);
    Result:=FFactory;
  finally
    EndRead;
  end;
end;

class function TTaurusTLS_SimpleAESFactory.GetKeySize: TTaurusTLS_AESKeySize;
begin
  Result:=GetFactory.FKeySize;
end;

class function TTaurusTLS_SimpleAESFactory.GetEncoderMode: TTaurusTLS_EncodeMode;
begin
  Result:=GetFactory.FEncodeMode;
end;

class function TTaurusTLS_SimpleAESFactory.GetKeySizeName(
  AKeySize: TTaurusTLS_AESKeySize): string;
begin
  Result:=cKeySizes[AKeySize];
end;

class function TTaurusTLS_SimpleAESFactory.GetEncodingModeName(
  AEncoderMode: TTaurusTLS_EncodeMode): string;
begin
  Result:=cEncoderModes[AEncoderMode];
end;

class function TTaurusTLS_SimpleAESFactory.NewEncryptor: TTaurusTLS_CustomEncryptor;
var
  lFactory: TTaurusTLS_SimpleAESFactory;

begin
  BeginRead;
  try
    lFactory:=FFactory;
    Result:=NewEncryptor(lFactory.FKeySize, lFactory.FEncodeMode);
  finally
    EndRead;
  end;
end;

class function TTaurusTLS_SimpleAESFactory.NewEncryptor(
  AKeySize: TTaurusTLS_AESKeySize;
  AEncoderMode: TTaurusTLS_EncodeMode): TTaurusTLS_CustomEncryptor;
begin
  Result:=TTaurusTLS_SimpleAESEncryptor.Create(AKeySize, AEncoderMode);
end;

class procedure TTaurusTLS_SimpleAESFactory.SetDefaultCipher(
  AKeySize: TTaurusTLS_AESKeySize; AEncoderMode: TTaurusTLS_EncodeMode);
begin
  BeginWrite;
  try
    FFactory.Free;
    FFactory:=TTaurusTLS_SimpleAESFactory.Create(AKeySize, AEncoderMode);
  finally
    EndWrite;
  end;
end;

end.
