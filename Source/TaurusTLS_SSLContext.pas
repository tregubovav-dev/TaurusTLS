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

  TTaurusTLS_CustomEncryptor = class;

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
    FEnv: TTaurusTLS_CustomEncryptor;

{$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} protected
    function NewBio: ITaurusTLS_Bio; override;
    procedure ReleaseNotify(const ASender: TTaurusTLS_CustomBytes);

    function DecryptBytes: TBytes;
    function EncrypBytes(ASrc: TBytes): TBytes;
    function GetPlainBytes: ITaurusTLS_Bytes;
  public
    constructor Create(ABytes: TBytes; AEncryptor: TTaurusTLS_CustomEncryptor);
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

  TTaurusTLS_CustomBytesHelper = class helper for TTaurusTLS_CustomBytes
  private
    class function NewBytes(ASize: NativeUInt): TBytes;
      static; {$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    class function LoadFromString(const ASrc: string): ITaurusTLS_Bytes;
      overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function LoadFromString(const ASrc: RawByteString): ITaurusTLS_Bytes;
      overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function LoadFromString(const ASrc: PIdAnsiChar): ITaurusTLS_Bytes;
      overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function LoadFromBytes(const ASrc: TBytes): ITaurusTLS_Bytes;
      static; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function LoadFromStream(const AStream: TStream): ITaurusTLS_Bytes;
      static; {$IFDEF USE_INLINE}inline;{$ENDIF}
  end;

  TTaurusTLS_EncryptedBytesHelper = class helper for TTaurusTLS_EncryptedBytes
  public
    class function LoadFromStream(const AStream: TStream; AWipeSrcMem: boolean;
      AEncryptor: TTaurusTLS_CustomEncryptor = nil):ITaurusTLS_Bytes;
      overload; static;
    class function LoadFromStream(const AStream: TStream; AWipeSrcMem: boolean;
      AKeySize: TTaurusTLS_AESKeySize; AEncodeMode: TTaurusTLS_EncodeMode):
      ITaurusTLS_Bytes; overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
  end;

  EETaurusTLSCipherError = class(ETaurusTLSAPICryptoError);
  EETaurusTLSEncryptorError = class(ETaurusTLSAPICryptoError);

(* --- To transfer consts to the TaurusTLSHeaders_evperr.pas --- *)
const
  EVP_R_INVALID_IV_LENGTH = 194;

(* --- To transfer resourcestrings to the TaurusTLS_ResourceStrings.pas --- *)
resourcestring
  REVP_Cipher_NoCipherProvided = 'Unable to initializate Cipher instance. '+
    'No OpenSSL Cipher provided.';
  REVP_Cipher_ZeroKeyLen = 'Unable to initializate Cipher instance with zero Key length.';
  REVP_Cipher_IVKeyLen = 'Unable to initializate Cipher instance with zero IV length.';

  REVP_Encryptor_CtxInitError = 'Error initialization Encrypt/Decrypt context.';
  REVP_EncryptDecrypt_InitNoCipher = 'Unable to initializate EncyptDecrypt instance. '+
        'No OpenSSL Cipher provided.';
  REVP_Encryptor_Encrypt_Error = 'Error in Encrypting data.';
  REVP_Encryptor_Decrypt_Error = 'Error in Decrypting data.';


implementation

uses
  TaurusTLSLoader,
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
  AEncryptor: TTaurusTLS_CustomEncryptor);
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
     EETaurusTLSCipherError.RaiseWithMessage(REVP_Cipher_NoCipherProvided);
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

{ TTaurusTLS_EncryptDecrypt }

constructor TTaurusTLS_CustomEncryptor.Create(ACipher: TTaurusTLS_Cipher);
begin
  inherited Create;
  if not Assigned(ACipher) then
    raise EETaurusTLSEncryptorError.Create(REVP_EncryptDecrypt_InitNoCipher);
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
    EETaurusTLSEncryptorError.RaiseException(REVP_Encryptor_Encrypt_Error);
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
    EETaurusTLSEncryptorError.RaiseException(REVP_Encryptor_Decrypt_Error);
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
    EETaurusTLSEncryptorError.RaiseException(REVP_Encryptor_CtxInitError);
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
var
  lFactory: TTaurusTLS_SimpleAESFactory;

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

{ TTaurusTLS_CustomBytesHelper }

class function TTaurusTLS_CustomBytesHelper.NewBytes(ASize: NativeUInt): TBytes;
begin
  if ASize > 0 then
    SetLength(Result, ASize);
end;

class function TTaurusTLS_CustomBytesHelper.LoadFromBytes(
  const ASrc: TBytes): ITaurusTLS_Bytes;
begin
  Result:=TTaurusTLS_PlainBytes.Create(ASrc);
end;

class function TTaurusTLS_CustomBytesHelper.LoadFromString(
  const ASrc: string): ITaurusTLS_Bytes;
begin
  Result:=LoadFromString(RawByteString(ASrc));
end;

class function TTaurusTLS_CustomBytesHelper.LoadFromString(
  const ASrc: RawByteString): ITaurusTLS_Bytes;
begin
  Result:=LoadFromString(PIdAnsiChar(ASrc));
end;

class function TTaurusTLS_CustomBytesHelper.LoadFromString(
  const ASrc: PIdAnsiChar): ITaurusTLS_Bytes;
begin
  Result:=TTaurusTLS_PlainBytes(BytesOf(ASrc));
end;

class function TTaurusTLS_CustomBytesHelper.LoadFromStream(
  const AStream: TStream): ITaurusTLS_Bytes;
var
  lStream: TBytesStream;
  lBytes: TBytes;

begin
  if not Assigned(AStream) then
    Exit(nil);

  if AStream is TBytesStream then
  begin
    lBytes:=TBytesStream(AStream).Bytes; // reuse TBytes from the underlyed stream
  end
  else
  begin
    lStream:=TBytesStream.Create(NewBytes(AStream.Size));
    try
      lStream.CopyFrom(AStream);
      lBytes:=lStream.Bytes;
    finally
      lStream.Free;
    end;
  end;
  TTaurusTLS_PlainBytes.Create(lBytes);
end;

type
  TStreamHelper = class helper for TStream
    procedure WipeMemoryData(AValue: integer = $0);
  end;

{ TStreamHelper }

procedure TStreamHelper.WipeMemoryData(AValue: integer = $0);
var
  lSize: UInt64;

begin
  if Self is TBytesStream then //TBytesStream inherits from TMemoryStream
    Exit;
  lSize:=Size;
  if (Self is TMemoryStream) and (lSize > 0) then
    FillChar(TMemoryStream(Self).Memory^, lSize, AValue);
end;


{ TTaurusTLS_EncryptedBytesHelper }

class function TTaurusTLS_EncryptedBytesHelper.LoadFromStream(
  const AStream: TStream; AWipeSrcMem: boolean;
  AEncryptor: TTaurusTLS_CustomEncryptor): ITaurusTLS_Bytes;
var
  lSize: UInt64;
  lStream: TBytesStream;
  lBytes: TBytes;

begin
  Result:=nil;
  if not Assigned(AStream) then
    Exit;
  if not Assigned(AEncryptor) then
    AEncryptor:=TTaurusTLS_SimpleAESFactory.NewEncryptor;
  lSize:= AStream.Size;
  if AStream is TBytesStream then
  begin
    lStream := TBytesStream(AStream); // will take ownership on underlyed array
    lBytes := lStream.Bytes;
  end
  else
  begin
    SetLength(lBytes, lSize);
    lStream := TBytesStream.Create(lBytes);
    try
      lStream.CopyFrom(AStream);
    finally
      if AWipeSrcMem then
        lStream.WipeMemoryData;
      lStream.Free;
    end;
  end;
  Result:=TTaurusTLS_EncryptedBytes.Create(lBytes, AEncryptor);
end;

class function TTaurusTLS_EncryptedBytesHelper.LoadFromStream(
  const AStream: TStream; AWipeSrcMem: boolean; AKeySize: TTaurusTLS_AESKeySize;
  AEncodeMode: TTaurusTLS_EncodeMode): ITaurusTLS_Bytes;
begin
  LoadFromStream(AStream, AWipeSrcMem,
    TTaurusTLS_SimpleAESFactory.NewEncryptor(AKeySize, AEncodeMode));
end;

end.
