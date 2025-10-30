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
///   Declares set of classes and interfaces to operate with
///   <see href="https://docs.openssl.org/3.5/man3/X509_STORE_add_cert/#description">X509_STORE</see>
/// </summary>

unit TaurusTLS_SSLStores;
{$I TaurusTLSLinkDefines.inc}

interface

uses
  SysUtils,
  Classes,
{$IFDEF DCC}
  System.Generics.Collections,
{$ENDIF}
  IdGlobal,
  IdCTypes,
  TaurusTLSHeaders_types,
  TaurusTLSHeaders_evp,
  TaurusTLSHeaders_pem,
  TaurusTLSHeaders_store,
  TaurusTLSHeaders_x509,
  TaurusTLSHeaders_x509_vfy,
  TaurusTLSExceptionHandlers,
  TaurusTLS_Encryptors,
  TaurusTLS_SSLContainers,
  TaurusTLS_SSLContainersHelpers,
  TaurusTLS_SSLUi;

type
  ETaurusTLSX509StoreError = class(ETaurusTLSAPICryptoError);
  ETaurusTLSX509ItemError  = class(ETaurusTLSAPICryptoError);
  ETaurusTLSX509CtxError   = class(ETaurusTLSAPICryptoError);

type
  ETaurusTLSOSSLStoreError = class(ETaurusTLSAPICryptoError);

  TTaurusTLS_OSSLStore = class
  public type
  TStoreInfoType = (sitNone=0, sitName=1, sitParams=2, sitPubKey=3,
    sitPrivKey=4, sitCert=5, sitCRL=6);
  TStoreInfoTypes = set of TStoreInfoType;

    TStoreInfoHelper = record helper for POSSL_STORE_INFO
    public
      class function GetType(AInfo: POSSL_STORE_INFO): TStoreInfoType; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function GetTypeName(AInfo: POSSL_STORE_INFO): PIdAnsiChar; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function IsExist(AInfo: POSSL_STORE_INFO): boolean; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function GetName(AInfo: POSSL_STORE_INFO): PIdAnsiChar; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function GetParams(AInfo: POSSL_STORE_INFO): PEVP_PKEY; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function GetPubKey(AInfo: POSSL_STORE_INFO): PEVP_PKEY; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function GetPrivKey(AInfo: POSSL_STORE_INFO): PEVP_PKEY; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function GetCert(AInfo: POSSL_STORE_INFO): PX509; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function GetCrl(AInfo: POSSL_STORE_INFO): PX509_CRL; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}

      class function CloneNameA(AInfo: POSSL_STORE_INFO): AnsiString; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function CloneNameW(AInfo: POSSL_STORE_INFO): UnicodeString; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function CloneParams(AInfo: POSSL_STORE_INFO): PEVP_PKEY; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function ClonePubKey(AInfo: POSSL_STORE_INFO): PEVP_PKEY; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function ClonePrivKey(AInfo: POSSL_STORE_INFO): PEVP_PKEY; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function CloneCert(AInfo: POSSL_STORE_INFO): PX509; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function CloneCrl(AInfo: POSSL_STORE_INFO): PX509_CRL; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}

      class procedure Free(var AInfo: POSSL_STORE_INFO); static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
    end;

    TStoreHelper = record helper for POSSL_STORE_CTX
    public
      class function Open(AUri: PIdAnsiChar; AUi: TTaurusTLS_CustomOsslUi): POSSL_STORE_CTX;
        overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function Open(ABio: ITaurusTLS_Bio; AUi: TTaurusTLS_CustomOsslUi): POSSL_STORE_CTX;
        overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
      class procedure Close(ACtx: POSSL_STORE_CTX); static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function Eof(ACtx: POSSL_STORE_CTX): boolean; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function IsLoadError(ACtx: POSSL_STORE_CTX): boolean; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function Load(ACtx: POSSL_STORE_CTX): POSSL_STORE_INFO; static;
    end;

    TStoreElement = sitName..sitCrl;
    TStoreElements = set of TStoreElement;

    TStoreItem = class
    private type
      TItemData = record
      private
        FName: AnsiString; // managed type can't be used in variant part of the record.
        case FType: TStoreInfoType of
        sitParams:  (FParams:   PEVP_PKEY);
        sitPubKey:  (FPubKey:   PEVP_PKEY);
        sitPrivKey: (FPrivKey:  PEVP_PKEY);
        sitCert:    (FCert:     PX509);
        sitCrl:     (FCrl:      PX509_CRL);
      end;
    private
      FData: TItemData;
        function GetType: TStoreInfoType; {$IFDEF USE_INLINE}inline;{$ENDIF}
        function GetName: AnsiString; {$IFDEF USE_INLINE}inline;{$ENDIF}
        function GetParams: PEVP_PKEY; {$IFDEF USE_INLINE}inline;{$ENDIF}
        function GetPubKey: PEVP_PKEY;  {$IFDEF USE_INLINE}inline;{$ENDIF}
        function GetPrivKey: PEVP_PKEY; {$IFDEF USE_INLINE}inline;{$ENDIF}
        function GetCert: PX509; {$IFDEF USE_INLINE}inline;{$ENDIF}
        function GetCrl: PX509_CRL; {$IFDEF USE_INLINE}inline;{$ENDIF}
    public
      constructor Create(AInfo: POSSL_STORE_INFO); overload;
      destructor Destroy; override;

      property &Type: TStoreInfoType read GetType;
      property Name: AnsiString read GetName;
      property Params: PEVP_PKEY read GetParams;
      property PubKey: PEVP_PKEY read GetPubKey;
      property PrivKey: PEVP_PKEY read GetPrivKey;
      property Cert: PX509 read GetCert;
      property Crl: PX509_CRL read GetCrl;
    end;

  public const
    cStoreAElementsll = [sitName..sitCRL];

  private type
    TCounters = array [TStoreElement] of TIdC_Uint;
    TListInfo = TObjectList<TStoreItem>;

    TFilterEnumerator = class
    private
      FList: TListInfo;
      FFilter: TStoreElements;
      FIdx: Integer;
    public
      constructor Create(AList: TListInfo; AFilter: TStoreElements);
      function GetCurrent: TStoreItem;
      function  MoveNext: boolean;
      property Current: TStoreItem read GetCurrent;
    end;

  private
    FCtx: POSSL_STORE_CTX;
    FList: TListInfo;
    FCounters: TCounters;
    FFilter: TStoreElements;
    function GetCount(AType: TStoreElement): TIdC_Uint;
      {$IFDEF USE_INLINE}inline;{$ENDIF}

  protected
    constructor Create(ACtx: POSSL_STORE_CTX;
        ALoadFilter: TStoreElements = cStoreAElementsll); overload;
    procedure DoException(AMessage: string);
    procedure DoLoad(ALoadFilter: TStoreElements);
  public
    constructor Create(AUri: AnsiString; AUi: TTaurusTLS_CustomOsslUi;
      ALoadFilter: TStoreElements = cStoreAElementsll); overload;
    constructor Create(AUri: UnicodeString; AUi: TTaurusTLS_CustomOsslUi;
      ALoadFilter: TStoreElements = cStoreAElementsll); overload;
    constructor Create(ABio: ITaurusTLS_Bio; AUi: TTaurusTLS_CustomOsslUi;
      ALoadFilter: TStoreElements = cStoreAElementsll); overload;
    destructor Destroy; override;

    function GetEnumerator: TFilterEnumerator;

    property Count[AType: TStoreElement]: TIdC_Uint read GetCount;
    property Filter: TStoreElements read FFilter write FFilter;
  end;

implementation

uses
  TaurusTLS_ResourceStrings;

{ TTaurusTLS_OsslStoreInfoHelper }

class function TTaurusTLS_OSSLStore.TStoreInfoHelper.GetType(AInfo:
    POSSL_STORE_INFO): TStoreInfoType;
begin
  Result:=TStoreInfoType(OSSL_STORE_INFO_get_type(AInfo));
end;

class function TTaurusTLS_OSSLStore.TStoreInfoHelper.GetTypeName(
  AInfo: POSSL_STORE_INFO): PIdAnsiChar;
begin
  if IsExist(Ainfo) then
    Result:=OSSL_STORE_INFO_type_string(Ord(GetType(AInfo)))
  else
    Result:=nil;
end;

class function TTaurusTLS_OSSLStore.TStoreInfoHelper.IsExist(
  AInfo: POSSL_STORE_INFO): boolean;
begin
  Result:=Assigned(AInfo);
end;

class function TTaurusTLS_OSSLStore.TStoreInfoHelper.GetName(
  AInfo: POSSL_STORE_INFO): PIdAnsiChar;
begin
  if IsExist(AInfo) then
    Result:=OSSL_STORE_INFO_get0_NAME(AInfo)
  else
    Result:=nil;
end;

class function TTaurusTLS_OSSLStore.TStoreInfoHelper.GetParams(
  AInfo: POSSL_STORE_INFO): PEVP_PKEY;
begin
  if IsExist(AInfo) then
    Result:=OSSL_STORE_INFO_get0_PARAMS(AInfo)
  else
    Result:=nil;
end;

class function TTaurusTLS_OSSLStore.TStoreInfoHelper.GetPubKey(
  AInfo: POSSL_STORE_INFO): PEVP_PKEY;
begin
  if IsExist(AInfo) then
    Result:=OSSL_STORE_INFO_get0_PUBKEY(AInfo)
  else
    Result:=nil;
end;

class function TTaurusTLS_OSSLStore.TStoreInfoHelper.GetPrivKey(
  AInfo: POSSL_STORE_INFO): PEVP_PKEY;
begin
  if IsExist(AInfo) then
    Result:=OSSL_STORE_INFO_get0_PKEY(AInfo)
  else
    Result:=nil;
end;

class function TTaurusTLS_OSSLStore.TStoreInfoHelper.GetCert(
  AInfo: POSSL_STORE_INFO): PX509;
begin
  if IsExist(AInfo) then
    Result:=OSSL_STORE_INFO_get0_CERT(AInfo)
  else
    Result:=nil;
end;

class function TTaurusTLS_OSSLStore.TStoreInfoHelper.GetCrl(
  AInfo: POSSL_STORE_INFO): PX509_CRL;
begin
  if IsExist(AInfo) then
    Result:=OSSL_STORE_INFO_get0_CRL(AInfo)
  else
    Result:=nil;
end;

class function TTaurusTLS_OSSLStore.TStoreInfoHelper.CloneNameA(
  AInfo: POSSL_STORE_INFO): AnsiString;
begin
  Result:=AnsiString(GetName(AInfo));
end;

class function TTaurusTLS_OSSLStore.TStoreInfoHelper.CloneNameW(
  AInfo: POSSL_STORE_INFO): UnicodeString;
begin
  Result:=UnicodeString(GetName(AInfo));
end;

class function TTaurusTLS_OSSLStore.TStoreInfoHelper.CloneParams(AInfo:
    POSSL_STORE_INFO): PEVP_PKEY;
begin
  if IsExist(AInfo) then
    Result:=OSSL_STORE_INFO_get1_PARAMS(AInfo)
  else
    Result:=nil;
end;

class function TTaurusTLS_OSSLStore.TStoreInfoHelper.ClonePubKey(
  AInfo: POSSL_STORE_INFO): PEVP_PKEY;
begin
  if IsExist(AInfo) then
    Result:=OSSL_STORE_INFO_get1_PUBKEY(AInfo)
  else
    Result:=nil;
end;

class function TTaurusTLS_OSSLStore.TStoreInfoHelper.ClonePrivKey(
  AInfo: POSSL_STORE_INFO): PEVP_PKEY;
begin
  if IsExist(AInfo) then
    Result:=OSSL_STORE_INFO_get1_PKEY(AInfo)
  else
    Result:=nil;
end;

class function TTaurusTLS_OSSLStore.TStoreInfoHelper.CloneCert(
  AInfo: POSSL_STORE_INFO): PX509;
begin
  if IsExist(AInfo) then
    Result:=OSSL_STORE_INFO_get1_CERT(AInfo)
  else
    Result:=nil;
end;

class function TTaurusTLS_OSSLStore.TStoreInfoHelper.CloneCrl(
  AInfo: POSSL_STORE_INFO): PX509_CRL;
begin
  if IsExist(AInfo) then
    Result:=OSSL_STORE_INFO_get1_CRL(AInfo)
  else
    Result:=nil;
end;

class procedure TTaurusTLS_OSSLStore.TStoreInfoHelper.Free(
  var AInfo: POSSL_STORE_INFO);
begin
  if IsExist(AInfo) then
    OSSL_STORE_INFO_free(AInfo);
  AInfo:=nil;
end;

{ TTaurusTLS_OSSLStore.TStoreHelper }

class function TTaurusTLS_OSSLStore.TStoreHelper.Open(AUri: PIdAnsiChar;
  AUi: TTaurusTLS_CustomOsslUi): POSSL_STORE_CTX;
var
  lMeth: PUI_METHOD;

begin
  if Assigned(AUi) then
    lMeth:=AUi.UiMethod
  else
  begin
    AUi:=nil;
    lMeth:=nil;
  end;

  Result:=OSSL_STORE_open(PIdAnsiChar(AUri), lMeth, AUi, nil, nil);
end;

class function TTaurusTLS_OSSLStore.TStoreHelper.Open(ABio: ITaurusTLS_Bio;
  AUi: TTaurusTLS_CustomOsslUi): POSSL_STORE_CTX;
var
  lMeth: PUI_METHOD;

begin
  Assert(Assigned(ABio), 'ABio must not be nil.');
  if Assigned(AUi) then
    lMeth:=AUi.UiMethod
  else
  begin
    AUi:=nil;
    lMeth:=nil;
  end;

  Result:=OSSL_STORE_attach(ABio.Bio, nil, nil, nil, lMeth, AUi,
    nil, nil, nil);
end;

class procedure TTaurusTLS_OSSLStore.TStoreHelper.Close(ACtx: POSSL_STORE_CTX);
begin
  OSSL_STORE_close(ACtx);
end;

class function TTaurusTLS_OSSLStore.TStoreHelper.Eof(
  ACtx: POSSL_STORE_CTX): boolean;
begin
  Result:=OSSL_STORE_eof(ACtx) = 1;
end;

class function TTaurusTLS_OSSLStore.TStoreHelper.IsLoadError(
  ACtx: POSSL_STORE_CTX): boolean;
begin
  Result:=OSSL_STORE_error(ACtx) = 1;
end;

class function TTaurusTLS_OSSLStore.TStoreHelper.Load(
  ACtx: POSSL_STORE_CTX): POSSL_STORE_INFO;
begin
  Result:=OSSL_STORE_load(ACtx);
end;

{ TTaurusTLS_OSSLStore.TStoreItem }

constructor TTaurusTLS_OSSLStore.TStoreItem.Create(AInfo: POSSL_STORE_INFO);
begin
  inherited Create;
  if Assigned(AInfo) then
    FData.FType:=POSSL_STORE_INFO.GetType(AInfo);
  case FData.FType of
    sitName:
      FData.FName:=POSSL_STORE_INFO.CloneNameA(AInfo);
    sitParams:
      FData.FParams:=POSSL_STORE_INFO.CloneParams(AInfo);
    sitPubKey:
      FData.FPubKey:=POSSL_STORE_INFO.ClonePubKey(AInfo);
    sitPrivKey:
      FData.FPrivKey:=POSSL_STORE_INFO.ClonePrivKey(AInfo);
    sitCert:
      FData.FCert:=POSSL_STORE_INFO.CloneCert(AInfo);
    sitCRL:
      FData.FCrl:=POSSL_STORE_INFO.CloneCrl(AInfo);
  end;
end;

destructor TTaurusTLS_OSSLStore.TStoreItem.Destroy;
var
  lKey: PEVP_PKEY;

begin
  case FData.FType of
    sitParams, sitPubKey, sitPrivKey:
    begin
      lKey:=nil;
      case FData.FType of
        sitParams:   lKey:=FData.FParams;
        sitPubKey:   lKey:=FData.FPubKey;
        sitPrivKey:  lKey:=FData.FPrivKey;
      end;
      EVP_PKEY_free(lKey);
    end;
    sitCert:
      X509_free(FData.FCert);
    sitCRL:
      X509_CRL_free(FData.FCrl);
  end;
  inherited;
end;

function TTaurusTLS_OSSLStore.TStoreItem.GetType: TStoreInfoType;
begin
  Result:=FData.FType;
end;

function TTaurusTLS_OSSLStore.TStoreItem.GetName: AnsiString;
begin
  Result:=FData.FName;
end;

function TTaurusTLS_OSSLStore.TStoreItem.GetParams: PEVP_PKEY;
begin
  if FData.FType = sitParams then
    Result:=FData.FParams
  else
    Result:=nil;
end;

function TTaurusTLS_OSSLStore.TStoreItem.GetPubKey: PEVP_PKEY;
begin
  if FData.FType = sitPubKey then
    Result:=FData.FPubKey
  else
    Result:=nil;
end;

function TTaurusTLS_OSSLStore.TStoreItem.GetPrivKey: PEVP_PKEY;
begin
  if FData.FType = sitPrivKey then
    Result:=FData.FPrivKey
  else
    Result:=nil;
end;

function TTaurusTLS_OSSLStore.TStoreItem.GetCert: PX509;
begin
  if FData.FType = sitCert then
    Result:=FData.FCert
  else
    Result:=nil;
end;

function TTaurusTLS_OSSLStore.TStoreItem.GetCrl: PX509_CRL;
begin
  if FData.FType = sitCRL then
    Result:=FData.FCrl
  else
    Result:=nil;
end;

{ TTaurusTLS_OSSLStore.TFilterEnumerator }

constructor TTaurusTLS_OSSLStore.TFilterEnumerator.Create(
  AList: TListInfo; AFilter: TStoreElements);
begin
  FList:=AList;
  FFilter:=AFilter;
  FIdx:=-1;
end;

function TTaurusTLS_OSSLStore.TFilterEnumerator.GetCurrent: TStoreItem;
begin
  Result:=FList[FIdx];
end;

function TTaurusTLS_OSSLStore.TFilterEnumerator.MoveNext: boolean;
begin
  while FIdx < FList.Count do
  begin
    Inc(FIdx);
    if (FIdx >= FList.Count) then
      Exit(False);
    if FList[FIdx].&Type in FFilter then
      Exit(True);
  end;
  Result:=False;
end;

{ TTaurusTLS_OSSLStore }

constructor TTaurusTLS_OSSLStore.Create(ACtx: POSSL_STORE_CTX;
  ALoadFilter: TStoreElements);
begin
  if not Assigned(ACtx) then
    DoException('OSSL Context was not initialized.');
  inherited Create;
  FList:=TListInfo.Create;
  FCtx:=ACtx;
  FFilter:= ALoadFilter;
  DoLoad(ALoadFilter);
end;

constructor TTaurusTLS_OSSLStore.Create(AUri: AnsiString;
  AUi:TTaurusTLS_CustomOsslUi; ALoadFilter: TStoreElements);
var
  lCtx: POSSL_STORE_CTX;

begin
  lCtx:=POSSL_STORE_CTX.Open(PIdAnsiChar(AUri), AUi);
  Create(lCtx, ALoadFilter);
end;

constructor TTaurusTLS_OSSLStore.Create(AUri: UnicodeString;
  AUi: TTaurusTLS_CustomOsslUi; ALoadFilter: TStoreElements);
begin
  Create(AnsiString(AUri), AUi, ALoadFilter);
end;

constructor TTaurusTLS_OSSLStore.Create(ABio: ITaurusTLS_Bio;
  AUi: TTaurusTLS_CustomOsslUi; ALoadFilter: TStoreElements);
var
  lCtx: POSSL_STORE_CTX;

begin
  lCtx:=POSSL_STORE_CTX.Open(ABio, AUi);
  Create(lCtx, ALoadFilter);
end;

destructor TTaurusTLS_OSSLStore.Destroy;
begin
  inherited;
  FreeAndNil(FList);
  if OSSL_STORE_close(Fctx) <> 1 then
    DoException('');
end;

function TTaurusTLS_OSSLStore.GetCount(AType: TStoreElement): TIdC_Uint;
begin
  Result:=FCounters[AType];
end;

procedure TTaurusTLS_OSSLStore.DoException(AMessage: string);
begin
  ETaurusTLSOSSLStoreError.RaiseException(AMessage);
end;

procedure TTaurusTLS_OSSLStore.DoLoad(ALoadFilter: TStoreElements);
var
  lCtx: POSSL_STORE_CTX;
  lFilter: TStoreElements;
  lInfo: POSSL_STORE_INFO;
  lItem: TStoreItem;

begin
  lCtx:=FCtx;
  lFilter:=FFilter;
  while not POSSL_STORE_CTX.Eof(lCtx) do
  begin
    lInfo:=POSSL_STORE_CTX.Load(lCtx);
    if not ((POSSL_STORE_INFO.IsExist(lInfo) and
      (POSSL_STORE_INFO.GetType(lInfo) in ALoadFilter))) then
      continue;
    try
      lItem:=TStoreItem.Create(lInfo);
      FList.Add(lItem);
      Inc(FCounters[lItem.&Type]);
    finally
      POSSL_STORE_INFO.Free(lInfo);
    end;
  end;
end;

function TTaurusTLS_OSSLStore.GetEnumerator: TFilterEnumerator;
begin
  Result:=TFilterEnumerator.Create(FList, FFilter);
end;

end.
