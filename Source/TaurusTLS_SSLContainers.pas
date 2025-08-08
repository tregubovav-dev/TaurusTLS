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

unit TaurusTLS_SSLContainers;
{$I TaurusTLSLinkDefines.inc}

interface

uses
  Classes, SysUtils, SyncObjs, TaurusTLS_Encryptors,
  TaurusTLSHeaders_types, TaurusTLSHeaders_bio, TaurusTLSExceptionHandlers,
  IdGlobal, IdCTypes;

type
  ETaurusTLSIBytesError = class(ETaurusTLSError);

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

  TTaurusTLS_Bytes = class (TInterfacedObject, ITaurusTLS_Bytes)
    protected type
      TBio = class(TInterfacedObject, ITaurusTLS_Bio)
      private
        FOwner: ITaurusTLS_Bytes;
        FBytes: TBytes;
        FBio: PBIO;
    {$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} protected
        function GetBio: PBIO;
      public
        constructor Create(ABytes: TBytes; AOwner: TTaurusTLS_Bytes);
        destructor Destroy; override;
      end;

  private
    FBytes: TBytes;
  protected
    function GetBytes: TBytes; virtual;
    function NewBio: ITaurusTLS_Bio; virtual;
    procedure SetBytes(const ABytes: TBytes); {$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    constructor Create(const ABytes: TBytes); overload;
    constructor Create(ASize: TIdC_SIZET); overload;
  end;

  TTaurusTLS_WipingBytes = class(TTaurusTLS_Bytes)
  protected
    procedure WipeData; overload;
  public
    class procedure WipeData(var AData: TBytes; AChar: Char = #0); overload;
      static; {$IFDEF USE_INLINE}inline;{$ENDIF}
    destructor Destroy; override;
  end;

  ITaurusTLS_BytesHost = interface
  ['{D4C93767-9DFB-4ADE-A8A0-4586CEB0A135}']
    procedure ReleaseNotify(const ASender: TTaurusTLS_Bytes);
  end;

  TTaurusTLS_EncryptedBytes = class(TInterfacedObject, ITaurusTLS_Bytes,
    ITaurusTLS_BytesHost)
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
    FEncryptedBytes: TBytes;
    FEncryptor: TTaurusTLS_CustomEncryptor;
{$IFDEF USE_STRICT_PRIVATE_PROTECTED}strict{$ENDIF} protected
    function GetBytes: TBytes;
    function NewBio: ITaurusTLS_Bio;
    procedure ReleaseNotify(const ASender: TTaurusTLS_Bytes);

    function DecryptBytes: TBytes;
    function EncrypBytes(ASrc: TBytes): TBytes;
    function GetPlainBytes: ITaurusTLS_Bytes;
    procedure SetBytes(const ABytes: TBytes);
  public
    constructor Create(ABytes: TBytes; AEncryptor: TTaurusTLS_CustomEncryptor);
    destructor Destroy; override;
  end;

  TTaurusTLS_BytesHelper = class helper for TTaurusTLS_Bytes
  private
    class function NewBytes(ASize: NativeUInt): TBytes;
      static; {$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    class function LoadFromString<T: TTaurusTLS_Bytes, constructor>(const ASrc: string):
      ITaurusTLS_Bytes; overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function LoadFromRawByteString<T: TTaurusTLS_Bytes, constructor>
      (const ASrc: RawByteString): ITaurusTLS_Bytes; overload; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function LoadFromPChar<T: TTaurusTLS_Bytes, constructor>(
      const ASrc: PIdAnsiChar): ITaurusTLS_Bytes; overload; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function LoadFromBytes<T: TTaurusTLS_Bytes, constructor>(
      const ASrc: TBytes): ITaurusTLS_Bytes; overload; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function LoadFromStream<T: TTaurusTLS_Bytes, constructor>(const AStream: TStream):
      ITaurusTLS_Bytes; overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
  end;

  TTaurusTLS_EncryptedBytesHelper = class helper for TTaurusTLS_EncryptedBytes
  public
    class function LoadFromStream(const AStream: TStream; AWipeSrcMem: boolean;
      AEncryptor: TTaurusTLS_CustomEncryptor = nil): ITaurusTLS_Bytes;
      overload; static;
    class function LoadFromStream(const AStream: TStream; AWipeSrcMem: boolean;
      AKeySize: TTaurusTLS_AESKeySize; AEncodeMode: TTaurusTLS_EncodeMode):
      ITaurusTLS_Bytes; overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
  end;

implementation

{ TTaurusTLS_CustomBytes.TBioIntf }

constructor TTaurusTLS_Bytes.TBio.Create(ABytes: TBytes;
  AOwner: TTaurusTLS_Bytes);
begin
  FOwner:=AOwner;
  FBytes:=ABytes;
end;

destructor TTaurusTLS_Bytes.TBio.Destroy;
begin
  if Assigned(FBio) then
    BIO_free(FBio);
  inherited;
end;

function TTaurusTLS_Bytes.TBio.GetBio: PBIO;
begin
  if not Assigned(FBio) and (Length(FBytes) > 0) then
    FBIO:=BIO_new_mem_buf(FBytes[0], Length(FBytes));
  Result:=FBIO;
end;

{ TTaurusTLS_Bytes }

constructor TTaurusTLS_Bytes.Create(const ABytes: TBytes);
begin
  Create;
  SetBytes(ABytes);
end;

constructor TTaurusTLS_Bytes.Create(ASize: TIdC_SIZET);
var
  lBytes: TBytes;

begin
  SetLength(lBytes, ASize);
  Create(lBytes);
end;

procedure TTaurusTLS_Bytes.SetBytes(const ABytes: TBytes);
begin
  if Length(FBytes) <> 0 then
    ETaurusTLSIBytesError.RaiseWithMessage('Error Message');
  FBytes:=ABytes;
end;

function TTaurusTLS_Bytes.GetBytes: TBytes;
begin
  Result:=FBytes;
end;

function TTaurusTLS_Bytes.NewBio: ITaurusTLS_Bio;
begin
  if Length(FBytes) > 0 then
    Result:=TBio.Create(FBytes, Self)
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
  WipeData(FBytes);
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
  inherited Create(ABytes);
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
  FEncryptor:=AEncryptor;
  SetBytes(ABytes);
end;

destructor TTaurusTLS_EncryptedBytes.Destroy;
begin
  FreeAndNil(FEncryptor);
  inherited;
end;

function TTaurusTLS_EncryptedBytes.DecryptBytes: TBytes;
begin
  FEncryptor.Decrypt(FEncryptedBytes, Result);
end;

function TTaurusTLS_EncryptedBytes.EncrypBytes(ASrc: TBytes): TBytes;
begin
  FEncryptor.Encrypt(ASrc, Result);
end;

function TTaurusTLS_EncryptedBytes.GetBytes: TBytes;
begin
  Result:=FEncryptedBytes;
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
    if TInterlocked.CompareExchange(Pointer(FPlainBytes), Pointer(lNewPlainBytes),
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

procedure TTaurusTLS_EncryptedBytes.ReleaseNotify(const ASender: TTaurusTLS_Bytes);
{$IFDEF DEBUG}
var
  lOldBytes: TTaurusTLS_Bytes;
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

procedure TTaurusTLS_EncryptedBytes.SetBytes(const ABytes: TBytes);
var
  lBytes: TBytes;

begin
  lBytes:=ABytes;
  try
    FEncryptedBytes:=EncrypBytes(ABytes);
  finally
    TPlainBytes.WipeData(lBytes); // clean-up source data always
  end;
end;

{ TTaurusTLS_CustomBytesHelper }

class function TTaurusTLS_BytesHelper.NewBytes(ASize: NativeUInt): TBytes;
begin
  if ASize > 0 then
    SetLength(Result, ASize);
end;

class function TTaurusTLS_BytesHelper.LoadFromBytes<T>(
  const ASrc: TBytes): ITaurusTLS_Bytes;
var
  lResult: T;

begin
  Result:=nil;
  lResult:=nil;
  try
    lResult:=T.Create;
    lResult.SetBytes(ASrc);
  except
    lResult.Free;
  end;
  Result:=lResult;
end;

class function TTaurusTLS_BytesHelper.LoadFromString<T>(
  const ASrc: string): ITaurusTLS_Bytes;
begin
  Result:=LoadFromRawByteString<T>(RawByteString(ASrc));
end;

class function TTaurusTLS_BytesHelper.LoadFromRawByteString<T>(
  const ASrc: RawByteString): ITaurusTLS_Bytes;
begin
  Result:=LoadFromPChar<T>(PIdAnsiChar(ASrc));
end;

class function TTaurusTLS_BytesHelper.LoadFromPchar<T>(
  const ASrc: PIdAnsiChar): ITaurusTLS_Bytes;
begin
  Result:=LoadFromBytes<T>(BytesOf(ASrc));
end;

class function TTaurusTLS_BytesHelper.LoadFromStream<T>(
  const AStream: TStream): ITaurusTLS_Bytes;
var
  lStream: TBytesStream;
  lBytes: TBytes;
  lResult: T;

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
  lResult:=nil;
  try
    lResult:=T.Create;
    lResult.SetBytes(lBytes);
    Result:=lResult;
  except
    lResult.Free;
  end;
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
