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
///   Classes wrapper on OpenSSL Random Generator functions.
/// </summary>
unit TaurusTLS_Random;
{$i TaurusTLSLinkDefines.inc}

interface

uses
  Classes,
  SysUtils,
  IdGlobal,
  IdCTypes,
  TaurusTLSExceptionHandlers,
  TaurusTLSHeaders_types,
  TaurusTLSHeaders_rand,
  TaurusTLSHeaders_randerr;

type
  TTaurusTLS_CustomOSSLRandomBytes = class
{$IFDEF USE_STRICT_PRIVATE_PROTECTED} strict{$ENDIF} private
    FCtx: POSSL_LIB_CTX;
    FStrength: TIdC_UINT;
{$IFDEF USE_STRICT_PRIVATE_PROTECTED} strict{$ENDIF} protected
    function DoRandom(ctx : POSSL_LIB_CTX; var buf; num: TIdC_SIZET;
      strength: TIdC_UINT): TIdC_INT; overload; virtual; abstract;
  public
    function Random(var buf; num: TIdC_SIZET): TIdC_INT;
      {$IFDEF USE_INLINE}inline; {$ENDIF}
    constructor Create(ACtx: POSSL_LIB_CTX;
      AStrength: TIdC_UINT = RAND_DEFAULT_STRENGTH);
  end;

  TTaurusTLS_OSSLPrivateRandomBytes = class(TTaurusTLS_CustomOSSLRandomBytes)
{$IFDEF USE_STRICT_PRIVATE_PROTECTED} strict{$ENDIF} protected
    function DoRandom(ctx : POSSL_LIB_CTX; var buf; num: TIdC_SIZET;
      strength: TIdC_UINT): TIdC_INT; override;
  end;

  TTaurusTLS_OSSLPublicRandomBytes = class(TTaurusTLS_CustomOSSLRandomBytes)
{$IFDEF USE_STRICT_PRIVATE_PROTECTED} strict{$ENDIF} protected
    function DoRandom(ctx : POSSL_LIB_CTX; var buf; num: TIdC_SIZET;
      strength: TIdC_UINT): TIdC_INT; override;
  end;

  TTaurusTLS_OSSLRandom = class
{$IFDEF USE_STRICT_PRIVATE_PROTECTED} strict{$ENDIF} private
    FRandomBytes: TTaurusTLS_CustomOSSLRandomBytes;

{$IFDEF USE_STRICT_PRIVATE_PROTECTED} strict{$ENDIF} private class var
    FPrivateRandom: TTaurusTLS_OSSLRandom;
    FPublicRandom: TTaurusTLS_OSSLRandom;

{$IFDEF USE_STRICT_PRIVATE_PROTECTED} strict{$ENDIF} protected
    class constructor Create;
    class destructor Destroy;
    constructor Create(ARandomGen: TTaurusTLS_CustomOSSLRandomBytes;
      ACtx: POSSL_LIB_CTX = nil; AStrength: TIdC_UINT = 0); overload;

    function GetRandom(var buf; num: TIdC_SIZET): TIdC_INT;
      {$IFDEF USE_INLINE}inline;{$ENDIF}

  public
    constructor Create; reintroduce; overload;
    function Random(var ABuffer; ASize: TIdC_SIZET): TIdC_INT;
      overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function Random(out ABytes: TBytes; ASize: TIdC_SIZET): TIdC_INT;
      overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function Random<T: record>(out AOut: T): TIdC_INT;
      overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function NewRandom(ARandomBytes: TTaurusTLS_CustomOSSLRandomBytes): TTaurusTLS_OSSLRandom;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    class property PrivateRandom: TTaurusTLS_OSSLRandom read FPrivateRandom;
    class property PublicRandom: TTaurusTLS_OSSLRandom read FPublicRandom;
  end;

  TTaurusTLS_Random = class
{$IFDEF USE_STRICT_PRIVATE_PROTECTED} strict{$ENDIF} private
    FRandomBytes: TTaurusTLS_CustomOSSLRandomBytes;

{$IFDEF USE_STRICT_PRIVATE_PROTECTED} strict{$ENDIF} private class var
    FPrivateRandom: TTaurusTLS_Random;
    FPublicRandom: TTaurusTLS_Random;

{$IFDEF USE_STRICT_PRIVATE_PROTECTED} strict{$ENDIF} protected
    class constructor Create;
    class destructor Destroy;
    constructor Create(ARandomGen: TTaurusTLS_CustomOSSLRandomBytes;
      ACtx: POSSL_LIB_CTX = nil; AStrength: TIdC_UINT = 0); overload;

    procedure GetRandom(var buf; num: TIdC_SIZET);
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure CheckError(const AResult: TIdC_INT);
      {$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    constructor Create; reintroduce; overload;
    procedure Random(var ABuffer; ASize: TIdC_SIZET);
      overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function Random(ASize: TIdC_SIZET): TBytes;
      overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function Random<T: record>: T;
      overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function NewRandom(ARandomBytes: TTaurusTLS_CustomOSSLRandomBytes): TTaurusTLS_Random;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    class property PrivateRandom: TTaurusTLS_Random read FPrivateRandom;
    class property PublicRandom: TTaurusTLS_Random read FPublicRandom;
  end;

  ERandom = class(ETaurusTLSAPICryptoError);

implementation

uses
  TaurusTLSHeaders_err;

{ TTaurusTLS_CustomOSSLRandomBytes }

constructor TTaurusTLS_CustomOSSLRandomBytes.Create(ACtx: POSSL_LIB_CTX;
  AStrength: TIdC_UINT);
begin
  FCtx:=ACtx;
  FStrength:=AStrength;
end;

function TTaurusTLS_CustomOSSLRandomBytes.Random(var buf;
  num: TIdC_SIZET): TIdC_INT;
begin
  Result:=DoRandom(FCtx, buf, num, FStrength);
end;

{ TTaurusTLS_OSSLPrivateRandomBytes }

function TTaurusTLS_OSSLPrivateRandomBytes.DoRandom(
  ctx: POSSL_LIB_CTX; var buf; num: TIdC_SIZET; strength: TIdC_UINT): TIdC_INT;
begin
  Result:=RAND_priv_bytes_ex(ctx, @buf, num, strength);
end;

{ TTaurusTLS_OSSLPublicRandomBytes }

function TTaurusTLS_OSSLPublicRandomBytes.DoRandom(
  ctx: POSSL_LIB_CTX; var buf; num: TIdC_SIZET; strength: TIdC_UINT): TIdC_INT;
begin
  Result:=RAND_bytes_ex(ctx, @buf, num, strength);
end;

{ TTaurusTLS_OSSLRandom }

class constructor TTaurusTLS_OSSLRandom.Create;
begin
  FPrivateRandom:=NewRandom(TTaurusTLS_OSSLPrivateRandomBytes.Create(nil, 0));
  FPublicRandom:=NewRandom(TTaurusTLS_OSSLPublicRandomBytes.Create(nil, 0));
end;

class destructor TTaurusTLS_OSSLRandom.Destroy;
begin
  FreeAndNil(FPublicRandom);
  FreeAndNil(FPrivateRandom);
end;

class function TTaurusTLS_OSSLRandom.NewRandom(
  ARandomBytes: TTaurusTLS_CustomOSSLRandomBytes): TTaurusTLS_OSSLRandom;
begin
  Result:=Create(ARandomBytes);
end;

constructor TTaurusTLS_OSSLRandom.Create;
begin
  Assert(False, ClassName+' can not be creates with this constructor.');
end;

constructor TTaurusTLS_OSSLRandom.Create(ARandomGen: TTaurusTLS_CustomOSSLRandomBytes;
  ACtx: POSSL_LIB_CTX; AStrength: TIdC_UINT);
begin
  Assert(Assigned(ARandomGen), 'ARandomGen must not be ''nil''.');
  FRandomBytes:=ARandomGen;
end;

function TTaurusTLS_OSSLRandom.GetRandom(var buf; num: TIdC_SIZET): TIdC_INT;
begin
  Result:=FRandomBytes.Random(buf, num);
end;

function TTaurusTLS_OSSLRandom.Random(var ABuffer;
  ASize: TIdC_SIZET): TIdC_INT;
begin
  Result:=GetRandom(ABuffer, ASize);
end;

function TTaurusTLS_OSSLRandom.Random(out ABytes: TBytes;
  ASize: TIdC_SIZET): TIdC_INT;
begin
  if ASize = 0 then
    Exit(1);
  try
    SetLength(ABytes, ASize);
  except
    //Push error function and error code to the OpenSSL errors stack.
    SSLErr(RAND_F_DRBG_BYTES, ERR_R_MALLOC_FAILURE);
  end;
  Result:=GetRandom(ABytes[0], ASize);
end;

function TTaurusTLS_OSSLRandom.Random<T>(out AOut: T): TIdC_INT;
begin
  Result:=GetRandom(AOut, SizeOf(T));
end;

{ TTaurusTLS_Random }

class constructor TTaurusTLS_Random.Create;
begin
  FPrivateRandom:=NewRandom(TTaurusTLS_OSSLPrivateRandomBytes.Create(nil, 0));
  FPublicRandom:=NewRandom(TTaurusTLS_OSSLPublicRandomBytes.Create(nil, 0));
end;

class destructor TTaurusTLS_Random.Destroy;
begin
  FreeAndNil(FPublicRandom);
  FreeAndNil(FPrivateRandom);
end;

class function TTaurusTLS_Random.NewRandom(
  ARandomBytes: TTaurusTLS_CustomOSSLRandomBytes): TTaurusTLS_Random;
begin
  Result:=Create(ARandomBytes);
end;

constructor TTaurusTLS_Random.Create;
begin
  Assert(False, ClassName+' can not be creates with this constructor.');
end;

procedure TTaurusTLS_Random.CheckError(const AResult: TIdC_INT);
begin
  if AResult <> 1 then
    raise ERandom.Create('');
end;

constructor TTaurusTLS_Random.Create(
  ARandomGen: TTaurusTLS_CustomOSSLRandomBytes; ACtx: POSSL_LIB_CTX;
  AStrength: TIdC_UINT);
begin
  Assert(Assigned(ARandomGen), 'ARandomGen must not be ''nil''.');
  FRandomBytes:=ARandomGen;
end;

procedure TTaurusTLS_Random.GetRandom(var buf; num: TIdC_SIZET);
begin
  CheckError(FRandomBytes.Random(buf, num));
end;

procedure TTaurusTLS_Random.Random(var ABuffer; ASize: TIdC_SIZET);
begin
  GetRandom(ABuffer, ASize);
end;

function TTaurusTLS_Random.Random(ASize: TIdC_SIZET): TBytes;
begin
  if ASize <= 0 then
    Exit;
  SetLength(Result, ASize);
  GetRandom(Result[0], ASize);
end;

function TTaurusTLS_Random.Random<T>: T;
begin
  GetRandom(Result, SizeOf(T));
end;

end.
