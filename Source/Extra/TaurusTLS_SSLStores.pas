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
  DateUtils,
  IdGlobal,
  IdCTypes,
  IdIpAddress,
  TaurusTLSHeaders_types,
  TaurusTLSHeaders_evp,
  TaurusTLSHeaders_pem,
  TaurusTLSHeaders_store,
  TaurusTLSHeaders_x509,
  TaurusTLSHeaders_x509v3,
  TaurusTLSHeaders_x509_vfy,
  TaurusTLSExceptionHandlers,
  TaurusTLS_Encryptors,
  TaurusTLS_SSLContainers,
  TaurusTLS_SSLContainersHelpers,
  TaurusTLS_SSLUi;

type
  ETaurusTLSX509StoreError = class(ETaurusTLSAPICryptoError);
  ETaurusTLSX509StoreThreadError = class(Exception);
  ETaurusTLSOSSLStoreError = class(ETaurusTLSAPICryptoError);

type
  TaurusTLS_CustomX509VerifyParam = class abstract
  public type
    TAuthLevel = 0..5;

    TVerifyFlag = (
      x509vfCheckTime                 = $01, // 1 shl $01 = X509_V_FLAG_USE_CHECK_TIME
      x509vfCheckCrl                  = $02, // 1 shl $02 = X509_V_FLAG_CRL_CHECK
      x509vfCheckCrlAll               = $03, // 1 shl $03 = X509_V_FLAG_CRL_CHECK_ALL
      x509vfIgnoreCritical            = $04, // 1 shl $04 = X509_V_FLAG_IGNORE_CRITICAL
      x509vfStrict                    = $05, // 1 shl $05 = X509_V_FLAG_X509_STRICT
      x509vfAllowProxyCerts           = $06, // 1 shl $06 = X509_V_FLAG_ALLOW_PROXY_CERTS
      x509vfPolicyCheck               = $07, // 1 shl $07 = X509_V_FLAG_POLICY_CHECK
      x509vfExplicitPolicy            = $08, // 1 shl $08 = X509_V_FLAG_EXPLICIT_POLICY
      x509vfInhibitAny                = $09, // 1 shl $09 = X509_V_FLAG_INHIBIT_ANY
      x509vfInhibitMap                = $0A, // 1 shl $0A = X509_V_FLAG_INHIBIT_MAP
      x509vfNotifyPolicy              = $0B, // 1 shl $0B = X509_V_FLAG_NOTIFY_POLICY
      x509vfExtendCrlSupport          = $0C, // 1 shl $0C = X509_V_FLAG_EXTENDED_CRL_SUPPORT
      x509vfUseCrlDeltas              = $0D, // 1 shl $0D = X509_V_FLAG_USE_DELTAS
      x509vfCheckSelfSignSignitures   = $0E, // 1 shl $0E = X509_V_FLAG_CHECK_SS_SIGNATURE
      x509vfTrustedFirst              = $0F, // 1 shl $0F = X509_V_FLAG_TRUSTED_FIRST
      x509vfSuiteB128Only             = $10, // 1 shl $10 = X509_V_FLAG_SUITEB_128_LOS_ONLY
      x509vfSuiteB192                 = $11, // 1 shl $11 = X509_V_FLAG_SUITEB_192_LOS
//      x509vfSuiteB128                         // X509_V_FLAG_SUITEB_128_LOS = X509_V_FLAG_SUITEB_128_LOS_ONLY + X509_V_FLAG_SUITEB_192_LOS
      x509vfPartialChain              = $13, // 1 shl $13 = X509_V_FLAG_PARTIAL_CHAIN
      x509vfNoAlternativeChain        = $14, // 1 shl $14 = X509_V_FLAG_NO_ALT_CHAINS
      x509vfNoCheckTime               = $15  // 1 shl $15 = X509_V_FLAG_NO_CHECK_TIME
    );
    TVerifyFlagHelper = record helper for TVerifyFlag
    private
      function GetAsInt: TIdC_ULONG; {$IFDEF USE_INLINE}inline;{$ENDIF}
      procedure SetAsInt(Value: TIdC_ULONG); {$IFDEF USE_INLINE}inline;{$ENDIF}
    public
      class function ToInt(Value: TVerifyFlag): TIdC_ULONG; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function FromInt(Value: TIdC_ULONG): TVerifyFlag; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      function IsEqualTo(Value: TIdC_ULONG): boolean;
        {$IFDEF USE_INLINE}inline;{$ENDIF}

      property AsInt: TIdC_ULONG read GetAsInt write SetAsInt;
    end;

    TVerifyFlags = set of TVerifyFlag;
    TVerifyFlagsHelper = record helper for TVerifyFlags
    private
      function GetAsInt: TIdC_ULONG; {$IFDEF USE_INLINE}inline;{$ENDIF}
      procedure SetAsInt(Value: TIdC_ULONG); {$IFDEF USE_INLINE}inline;{$ENDIF}
    public
      class function ToInt(Value: TVerifyFlags): TIdC_ULONG; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function FromInt(Value: TIdC_ULONG): TVerifyFlags; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      function IsEqualTo(Value: TIdC_ULONG): boolean;
        {$IFDEF USE_INLINE}inline;{$ENDIF}

      property AsInt: TIdC_ULONG read GetAsInt write SetAsInt;
    end;

    TInheritanceFlag = (
      x509ihfDefault                  = $0, // 1 shl $0 = X509_VP_FLAG_DEFAULT
      x509ihfOverrite                 = $1, // 1 shl $1 = X509_VP_FLAG_OVERWRITE
      x509ihfReset                    = $2, // 1 shl $2 = X509_VP_FLAG_RESET_FLAGS
      x509ihfLocked                   = $3, // 1 shl $3 = X509_VP_FLAG_LOCKED
      x509ihfOnce                     = $4  // 1 shl $4 = X509_VP_FLAG_ONCE
    );
    TInheritanceFlagHelper = record helper for TInheritanceFlag
    private
      function GetAsInt: TIdC_UINT32; {$IFDEF USE_INLINE}inline;{$ENDIF}
      procedure SetAsInt(Value: TIdC_UINT32); {$IFDEF USE_INLINE}inline;{$ENDIF}
    public
      class function ToInt(Value: TInheritanceFlag): TIdC_UINT32; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function FromInt(Value: TIdC_UINT32): TInheritanceFlag; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      function IsEqualTo(Value: TIdC_UINT32): boolean;
        {$IFDEF USE_INLINE}inline;{$ENDIF}

      property AsInt: TIdC_UINT32 read GetAsInt write SetAsInt;
    end;

    TInheritanceFlags = set of TInheritanceFlag;
    TInheritanceFlagsHelper = record helper for TInheritanceFlags
    private
      function GetAsInt: TIdC_UINT32; {$IFDEF USE_INLINE}inline;{$ENDIF}
      procedure SetAsInt(Value: TIdC_UINT32); {$IFDEF USE_INLINE}inline;{$ENDIF}
    public
      class function ToInt(Value: TInheritanceFlags): TIdC_UINT32; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function FromInt(Value: TIdC_UINT32): TInheritanceFlags; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      function IsEqualTo(Value: TIdC_UINT32): boolean;
        {$IFDEF USE_INLINE}inline;{$ENDIF}

      property AsInt: TIdC_UINT32 read GetAsInt write SetAsInt;
    end;

    TTrustFlag = (
      trDefault     = X509_TRUST_DEFAULT,
      trCompat      = X509_TRUST_COMPAT,
      trSslClient   = X509_TRUST_SSL_CLIENT,
      trSslServer   = X509_TRUST_SSL_SERVER,
      trEMail       = X509_TRUST_EMAIL,
      trObjectSign  = X509_TRUST_OBJECT_SIGN,
      trOspSign     = X509_TRUST_OCSP_SIGN,
      trOspReq      = X509_TRUST_OCSP_REQUEST,
      trTsa         = X509_TRUST_TSA
    );
    TTrustFlagHelper = record helper for TTrustFlag
    private
      function GetAsInt: TIdC_Int; {$IFDEF USE_INLINE}inline;{$ENDIF}
      procedure SetAsInt(Value: TIdC_Int); {$IFDEF USE_INLINE}inline;{$ENDIF}
    public
      class function ToInt(Value: TTrustFlag): TIdC_Int; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function FromInt(Value: TIdC_Int): TTrustFlag; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      function IsEqualTo(Value: TIdC_Int): boolean;
        {$IFDEF USE_INLINE}inline;{$ENDIF}

      property AsInt: TIdC_Int read GetAsInt write SetAsInt;
    end;

    TPurpose = (
      prpDefaultAny     = 0, //X509_PURPOSE_DEFAULT_ANY
      prpSslClient      = X509_PURPOSE_SSL_CLIENT,
      prpSslServer      = X509_PURPOSE_SSL_SERVER,
      prpNsSSLServer    = X509_PURPOSE_NS_SSL_SERVER,
      prpSMimeSign      = X509_PURPOSE_SMIME_SIGN,
      prpSMimeEncrypt   = X509_PURPOSE_SMIME_ENCRYPT,
      prpCrlSign        = X509_PURPOSE_CRL_SIGN,
      prpAny            = X509_PURPOSE_ANY,
      prpOspHelper      = X509_PURPOSE_OCSP_HELPER,
      prpTimeStampSign  = X509_PURPOSE_TIMESTAMP_SIGN,
      prpCodeSign       = X509_PURPOSE_CODE_SIGN
    );
    TPurposeHelper = record helper for TPurpose
    private
      function GetAsInt: TIdC_Int; {$IFDEF USE_INLINE}inline;{$ENDIF}
      procedure SetAsInt(Value: TIdC_Int); {$IFDEF USE_INLINE}inline;{$ENDIF}
    public
      class function ToInt(Value: TPurpose): TIdC_Int; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function FromInt(Value: TIdC_Int): TPurpose; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      function IsEqualTo(Value: TIdC_Int): boolean;
        {$IFDEF USE_INLINE}inline;{$ENDIF}

      property AsInt: TIdC_Int read GetAsInt write SetAsInt;
    end;

    THostCheckFlag = (
      hckAlwaysChkSubj      = $0, // 1 shl $0 = X509_CHECK_FLAG_ALWAYS_CHECK_SUBJECT
      hckNoWildcard         = $1, // 1 shl $1 = X509_CHECK_FLAG_NO_WILDCARDS
      hckNoPartWildcard     = $2, // 1 shl $2 = X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS
      hckMultiLblWildcard   = $3, // 1 shl $3 = X509_CHECK_FLAG_MULTI_LABEL_WILDCARDS
      hckSingleLblSubDomain = $4  // 1 shl $4 = X509_CHECK_FLAG_SINGLE_LABEL_SUBDOMAINS
    );
    THostCheckFlagHelper = record helper for THostCheckFlag
    private
      function GetAsInt: TIdC_UINT; {$IFDEF USE_INLINE}inline;{$ENDIF}
      procedure SetAsInt(Value: TIdC_UINT); {$IFDEF USE_INLINE}inline;{$ENDIF}
    public
      class function ToInt(Value: THostCheckFlag): TIdC_UINT; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function FromInt(Value: TIdC_UINT): THostCheckFlag; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      function IsEqualTo(Value: TIdC_UINT): boolean;
        {$IFDEF USE_INLINE}inline;{$ENDIF}

      property AsInt: TIdC_UINT read GetAsInt write SetAsInt;
    end;

   THostCheckFlags = set of THostCheckFlag;
   THostCheckFlagsHelper = record helper for THostCheckFlags
    private
      function GetAsInt: TIdC_UINT; {$IFDEF USE_INLINE}inline;{$ENDIF}
      procedure SetAsInt(Value: TIdC_UINT); {$IFDEF USE_INLINE}inline;{$ENDIF}
    public
      class function ToInt(Value: THostCheckFlags): TIdC_UINT; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      class function FromInt(Value: TIdC_UINT): THostCheckFlags; static;
        {$IFDEF USE_INLINE}inline;{$ENDIF}
      function IsEqualTo(Value: TIdC_UINT): boolean;
        {$IFDEF USE_INLINE}inline;{$ENDIF}

      property AsInt: TIdC_UINT read GetAsInt write SetAsInt;
   end;

  public const
    cX509vfMask = X509_V_FLAG_USE_CHECK_TIME or X509_V_FLAG_CRL_CHECK
      or X509_V_FLAG_CRL_CHECK_ALL or X509_V_FLAG_IGNORE_CRITICAL
      or X509_V_FLAG_X509_STRICT or X509_V_FLAG_ALLOW_PROXY_CERTS
      or X509_V_FLAG_POLICY_CHECK or X509_V_FLAG_EXPLICIT_POLICY
      or X509_V_FLAG_INHIBIT_ANY or X509_V_FLAG_INHIBIT_MAP
      or X509_V_FLAG_NOTIFY_POLICY or X509_V_FLAG_EXTENDED_CRL_SUPPORT
      or X509_V_FLAG_USE_DELTAS or X509_V_FLAG_CHECK_SS_SIGNATURE
      or X509_V_FLAG_TRUSTED_FIRST or X509_V_FLAG_SUITEB_128_LOS_ONLY
      or X509_V_FLAG_SUITEB_192_LOS or X509_V_FLAG_SUITEB_128_LOS
      or X509_V_FLAG_PARTIAL_CHAIN or X509_V_FLAG_NO_ALT_CHAINS
      or X509_V_FLAG_NO_CHECK_TIME;

    cX509ihfMask = X509_VP_FLAG_DEFAULT or X509_VP_FLAG_OVERWRITE
      or X509_VP_FLAG_RESET_FLAGS or X509_VP_FLAG_LOCKED or X509_VP_FLAG_ONCE;

    cX509hckMask = X509_CHECK_FLAG_ALWAYS_CHECK_SUBJECT
      or X509_CHECK_FLAG_NO_WILDCARDS or X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS
      or X509_CHECK_FLAG_MULTI_LABEL_WILDCARDS or X509_CHECK_FLAG_SINGLE_LABEL_SUBDOMAINS;

  private
    FParam: PX509_VERIFY_PARAM ;
    function GetVerifyFlags: TVerifyFlags;
    procedure SetVerifyFlags(const Value: TVerifyFlags);
    function GetInheritanceFlags: TInheritanceFlags;
    procedure SetInheritanceFlags(const Value: TInheritanceFlags);
    function GetDepht: TIdC_Int;
    procedure SetDepth(const Value: TIdC_Int);
    function GetAuthLevel: TAuthLevel;
    procedure SetAuthLevel(const Value: TAuthLevel);
    function GetTime: TDateTime;
    procedure SetTime(const Value: TDateTime);
    function GetHostCheckFlags: THostCheckFlags;
    procedure SetHostCheckFlags(const Value: THostCheckFlags);
    function GetPurpose: TPurpose;
    procedure SetPurpose(Value: TPurpose);

  protected
    constructor Create(AParam: PX509_VERIFY_PARAM);
    procedure DoException(AMessage: string);

    property VfyParam: PX509_VERIFY_PARAM read FParam;
  public
    function GetHostRaw(ANumber: TIdC_Int): PIdAnsiChar;
    function GetHostA(ANumber: TIdC_Int): RawByteString;
    function GetHostW(ANumber: TIdC_Int): UnicodeString;
    procedure SetHostA(Value: RawByteString);
    procedure SetHostW(Value: UnicodeString);
    procedure AddHostA(Value: RawByteString);
    procedure AddHostW(Value: UnicodeString);
    function GetPerNameA: RawByteString;
    function GetPerNameW: UnicodeString;
    function GetEmailRaw: PIdAnsiChar;
    function GetEmailA: RawByteString;
    function GetEmailW: UnicodeString;
    procedure SetEMailA(Value: RawByteString);
    procedure SetEMailW(Value: UnicodeString);
    procedure SetIpAddress(Value: TIdIPAddress);
    procedure SetIpAddressA(Value: RawByteString);
    procedure SetIpAddressW(Value: UnicodeString);
    function GetIpAddressA: RawByteString;
    function GetIpAddressW: UnicodeString;

    property VerifyFlags: TVerifyFlags read GetVerifyFlags write SetVerifyFlags;
    property InheritanceFlags: TInheritanceFlags read GetInheritanceFlags
      write SetInheritanceFlags;
    property HostCheckFlags: THostCheckFlags read GetHostCheckFlags
      write SetHostCheckFlags;
    property Purpose: TPurpose read GetPurpose write SetPurpose;
    property Depth: TIdC_Int read GetDepht write SetDepth;
    property AuthLevel: TAuthLevel read GetAuthLevel write SetAuthLevel;
    property Time: TDateTime read GetTime write SetTime;
{$IFDEF DCC}
    property PerName: UnicodeString read GetPerNameW;
    property Host[i: TIdC_Int]: UnicodeString read GetHostW;
    property Email: UnicodeString read GetEmailW;
    property IPAddress: UnicodeString read GetIpAddressW;
{$ENDIF}
{$IFDEF FPC}
    property PerName: UTF8String read GetPerNameA;
    property Host[i: TIdC_Int]: RawbyteString read GetHostA;
    property Email: RawbyteString read GetEmailW;
    property IPAddress: UnicodeString read GetIpAddressA;
{$ENDIF}
  end;

  TaurusTLS_X509VerifyParam = class(TaurusTLS_CustomX509VerifyParam)
  public
    constructor Create;
    destructor Destroy; override;
  end;

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
        FName: RawByteString; // managed type can't be used in variant part of the record.
        case FType: TStoreInfoType of
        sitParams,
        sitPubKey,
        sitPrivKey: (FPKey:   PEVP_PKEY);
        sitCert:    (FCert:     PX509);
        sitCrl:     (FCrl:      PX509_CRL);
      end;
    private
      FData: TItemData;
      function GetType: TStoreInfoType; {$IFDEF USE_INLINE}inline;{$ENDIF}
      function GetName: RawByteString; {$IFDEF USE_INLINE}inline;{$ENDIF}
      function GetParams: PEVP_PKEY; {$IFDEF USE_INLINE}inline;{$ENDIF}
      function GetPubKey: PEVP_PKEY;  {$IFDEF USE_INLINE}inline;{$ENDIF}
      function GetPrivKey: PEVP_PKEY; {$IFDEF USE_INLINE}inline;{$ENDIF}
      function GetCert: PX509; {$IFDEF USE_INLINE}inline;{$ENDIF}
      function GetCrl: PX509_CRL; {$IFDEF USE_INLINE}inline;{$ENDIF}
    public
      constructor Create(AInfo: POSSL_STORE_INFO); overload;
      destructor Destroy; override;

      property &Type: TStoreInfoType read GetType;
      property Name: RawByteString read GetName;
      property Params: PEVP_PKEY read GetParams;
      property PubKey: PEVP_PKEY read GetPubKey;
      property PrivKey: PEVP_PKEY read GetPrivKey;
      property Cert: PX509 read GetCert;
      property Crl: PX509_CRL read GetCrl;
    end;

  public const
    cStoreAElementsAll = [sitName..sitCRL];

  private type
    TCounters = array [TStoreElement] of TIdC_Uint;
    TListInfo = TObjectList<TStoreItem>;

  private
//    FStore: POSSL_STORE_CTX;
    FList: TListInfo;
    FCounters: TCounters;
//    FFilter: TStoreElements;
    function GetCount(AType: TStoreElement): TIdC_Uint;
      {$IFDEF USE_INLINE}inline;{$ENDIF}

  protected
    constructor Create(ACtx: POSSL_STORE_CTX;
        ALoadFilter: TStoreElements = cStoreAElementsAll); overload;
    procedure DoException(AMessage: string);
    procedure DoLoad(ACtx: POSSL_STORE_CTX; ALoadFilter: TStoreElements);

//    property Store: POSSL_STORE_CTX read FStore;
  public
    constructor Create(AUri: RawByteString; AUi: TTaurusTLS_CustomOsslUi;
      ALoadFilter: TStoreElements = cStoreAElementsAll); overload;
    constructor Create(AUri: UnicodeString; AUi: TTaurusTLS_CustomOsslUi;
      ALoadFilter: TStoreElements = cStoreAElementsAll); overload;
    constructor Create(ABio: ITaurusTLS_Bio; AUi: TTaurusTLS_CustomOsslUi;
      ALoadFilter: TStoreElements = cStoreAElementsAll); overload;
    destructor Destroy; override;

    property Count[AType: TStoreElement]: TIdC_Uint read GetCount;
  end;

  TTaurusTLS_OSSLStoreHelper = class helper for TTaurusTLS_OSSLStore
  public type
    TEnumerator = class
    private
      FEnum: TListInfo.TEnumerator;
      FFilter: TStoreElements;
      FCurrent: TStoreItem;
    protected
      function GetCurrent: TStoreItem;
    public
      constructor Create(AList: TListInfo; AFilter: TStoreElements);
      destructor Destroy; override;
      function GetEnumerator: TEnumerator;
      function MoveNext: boolean;
      property Current: TStoreItem read GetCurrent;
    end;
  public
    function GetEnumerator: TEnumerator; overload;
    function GetEnumerator(const AFilter: TTaurusTLS_OSSLStore.TStoreElements):
      TEnumerator; overload;
  end;

  TaurusTLS_X509Store = class
  public type
    TX509Element = sitCert..sitCRL;
    TX509Elements = set of TX509Element;

  public const
    cX509ElementsAll = [sitCert..sitCRL];

  protected type
    TVfyParam = class(TaurusTLS_CustomX509VerifyParam)
    public
      constructor Create(AStore: PX509_STORE);
    end;

  private
    FStore: PX509_STORE;
    FVfyParam: TaurusTLS_CustomX509VerifyParam;
    procedure SetParam(AVfyParam: TaurusTLS_CustomX509VerifyParam);
    function GetParam: TaurusTLS_CustomX509VerifyParam;
  protected
    procedure DoException(AMessage: string); {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Store: PX509_STORE read FStore;
  public
    constructor Create; overload;
    constructor Create(AStore: TTaurusTLS_OSSLStore; AFilter: TX509Elements);
      overload;
    destructor Destroy; override;

    procedure AddCert(ACert: PX509); {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure AddCrl(ACrl: PX509_CRL); {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure LoadFromStore(AStore: TTaurusTLS_OSSLStore; AFilter: TX509Elements);
      overload;

    property VfyParam: TaurusTLS_CustomX509VerifyParam read GetParam write SetParam;
  end;

implementation

uses
  TaurusTLSHeaders_crypto, TaurusTLS_ResourceStrings;

{ TaurusTLS_CustomX509VerifyParam.TVerifyFlagHelper }

function TaurusTLS_CustomX509VerifyParam.TVerifyFlagHelper.GetAsInt: TIdC_ULONG;
begin
  Result:=ToInt(Self);
end;

procedure TaurusTLS_CustomX509VerifyParam.TVerifyFlagHelper.SetAsInt(
  Value: TIdC_ULONG);
begin
  Self:=FromInt(Value);
end;

class function TaurusTLS_CustomX509VerifyParam.TVerifyFlagHelper.ToInt(
  Value: TVerifyFlag): TIdC_ULONG;
begin
  Result:=TIdC_ULONG(1 shl Ord(Value)) and cX509vfMask;
end;

class function TaurusTLS_CustomX509VerifyParam.TVerifyFlagHelper.FromInt(
  Value: TIdC_ULONG): TVerifyFlag;
var
  i: TIdC_ULONG;

begin
  // check if AVal in range of OpenSSL X509_V_FLAG_* constants
  // and a single bit is set.
  if (Value = 0) or ((Value and (Value-1)) <> 0)
    or ((Value or cX509vfMask) <> cX509vfMask) then
    raise EInvalidCast.Create('Invalid X509 Verify Flag.');
  i:=Ord(Low(TVerifyFlag));
  while (1 shl i) < Value do
    Inc(i);
  Result:=TVerifyFlag(i);
end;

function TaurusTLS_CustomX509VerifyParam.TVerifyFlagHelper.IsEqualTo(
  Value: TIdC_ULONG): boolean;
begin
  Result:=Value = AsInt;
end;

{ TaurusTLS_X509VerifyParam.TVerifyFlagsHelper }

function TaurusTLS_CustomX509VerifyParam.TVerifyFlagsHelper.GetAsInt: TIdC_ULONG;
begin
  Result:=ToInt(Self);
end;

procedure TaurusTLS_CustomX509VerifyParam.TVerifyFlagsHelper.SetAsInt(
  Value: TIdC_ULONG);
begin
  Self:=FromInt(Value);
end;

class function TaurusTLS_CustomX509VerifyParam.TVerifyFlagsHelper.ToInt(
  Value: TVerifyFlags): TIdC_ULONG;
begin
  Result:=(TIdC_ULONG((@Value)^) and cX509vfMask);
end;

class function TaurusTLS_CustomX509VerifyParam.TVerifyFlagsHelper.FromInt(
  Value: TIdC_ULONG): TVerifyFlags;
begin
  if (Value or cX509vfMask) <> cX509vfMask then
    raise EInvalidCast.Create('Invalid X509 Verify Flags.');
{$I RangeCheck-OFF.inc}
  Result:=TVerifyFlags((@Value)^);
{$I RangeCheck-ON.inc}
end;

function TaurusTLS_CustomX509VerifyParam.TVerifyFlagsHelper.IsEqualTo(
  Value: TIdC_ULONG): boolean;
begin
  Result:=Value = AsInt;
end;

{ TaurusTLS_CustomX509VerifyParam.TInheritanceFlagHelper }

function TaurusTLS_CustomX509VerifyParam.TInheritanceFlagHelper.GetAsInt: TIdC_UINT32;
begin
  Result:=ToInt(Self);
end;

procedure TaurusTLS_CustomX509VerifyParam.TInheritanceFlagHelper.SetAsInt(
  Value: TIdC_UINT32);
begin
  Self:=FromInt(Value);
end;

class function TaurusTLS_CustomX509VerifyParam.TInheritanceFlagHelper.ToInt(
  Value: TInheritanceFlag): TIdC_UINT32;
begin
  Result:=TIdC_UINT32(1 shl Ord(Value)) and cX509ihfMask;
end;

class function TaurusTLS_CustomX509VerifyParam.TInheritanceFlagHelper.FromInt(
  Value: TIdC_UINT32): TInheritanceFlag;
var
  i: TIdC_UINT32;

begin
  // check if AVal in range of OpenSSL X509_VP_FLAG_* constants
  // and a single bit is set.
  if (Value = 0) or ((Value and (Value-1)) <> 0)
    or ((Value or cX509ihfMask) <> cX509ihfMask) then
    raise EInvalidCast.Create('Invalid X509 Inheritance Flag.');
  i:=Ord(Low(TInheritanceFlag));
  while (1 shl i) < Value do
    Inc(i);
{$I RangeCheck-OFF.inc}
  Result:=TInheritanceFlag(i);
{$I RangeCheck-ON.inc}
end;

function TaurusTLS_CustomX509VerifyParam.TInheritanceFlagHelper.IsEqualTo(
  Value: TIdC_UINT32): boolean;
begin
  Result:=Value = AsInt;
end;

{ TaurusTLS_CustomX509VerifyParam.TInheritanceFlagsHelper }

function TaurusTLS_CustomX509VerifyParam.TInheritanceFlagsHelper.GetAsInt: TIdC_UINT32;
begin
  Result:=ToInt(Self);
end;

procedure TaurusTLS_CustomX509VerifyParam.TInheritanceFlagsHelper.SetAsInt(
  Value: TIdC_UINT32);
begin
  Self:=FromInt(Value);
end;

class function TaurusTLS_CustomX509VerifyParam.TInheritanceFlagsHelper.ToInt(
  Value: TInheritanceFlags): TIdC_UINT32;
begin
  Result:=(TIdC_UINT32((@Value)^) and cX509ihfMask);
end;

class function TaurusTLS_CustomX509VerifyParam.TInheritanceFlagsHelper.FromInt(
  Value: TIdC_UINT32): TInheritanceFlags;
begin
  if (Value or cX509ihfMask) <> cX509ihfMask then
    raise EInvalidCast.Create('Invalid X509 Inheritance Flags.');
{$I RangeCheck-OFF.inc}
  Result:=TInheritanceFlags((@Value)^);
{$I RangeCheck-ON.inc}
end;

function TaurusTLS_CustomX509VerifyParam.TInheritanceFlagsHelper.IsEqualTo(
  Value: TIdC_UINT32): boolean;
begin
  Result:=Value = AsInt;
end;

{ TaurusTLS_CustomX509VerifyParam.TTrustFlagHelper }

function TaurusTLS_CustomX509VerifyParam.TTrustFlagHelper.GetAsInt: TIdC_Int;
begin
  Result:=ToInt(Self);
end;

procedure TaurusTLS_CustomX509VerifyParam.TTrustFlagHelper.SetAsInt(
  Value: TIdC_Int);
begin
  Self:=FromInt(Value);
end;

class function TaurusTLS_CustomX509VerifyParam.TTrustFlagHelper.FromInt(
  Value: TIdC_Int): TTrustFlag;
begin
  if (Value < Ord(Low(TTrustFlag))) or (Value > Ord(High(TTrustFlag))) then
    raise EInvalidCast.Create('Invalid X509 Trust Flag.');
{$I RangeCheck-OFF.inc}
  Result:=TTrustFlag(Value);
{$I RangeCheck-ON.inc}
end;

class function TaurusTLS_CustomX509VerifyParam.TTrustFlagHelper.ToInt(
  Value: TTrustFlag): TIdC_Int;
begin
  Result:=Ord(Value);
end;

function TaurusTLS_CustomX509VerifyParam.TTrustFlagHelper.IsEqualTo(
  Value: TIdC_Int): boolean;
begin
  Result:=Value = AsInt;
end;

{ TaurusTLS_CustomX509VerifyParam.TPurposeHelper }

function TaurusTLS_CustomX509VerifyParam.TPurposeHelper.GetAsInt: TIdC_Int;
begin
  Result:=ToInt(Self);
end;

procedure TaurusTLS_CustomX509VerifyParam.TPurposeHelper.SetAsInt(
  Value: TIdC_Int);
begin
  Self:=FromInt(Value);
end;

class function TaurusTLS_CustomX509VerifyParam.TPurposeHelper.FromInt(
  Value: TIdC_Int): TPurpose;
begin
  if (Value < Ord(Low(TPurpose))) or (Value > Ord(High(TPurpose))) then
    raise EInvalidCast.Create('Invalid X509 Trust Flag.');
{$I RangeCheck-OFF.inc}
  Result:=TPurpose(Value);
{$I RangeCheck-ON.inc}
end;

class function TaurusTLS_CustomX509VerifyParam.TPurposeHelper.ToInt(
  Value: TPurpose): TIdC_Int;
begin
  Result:=Ord(Value);
end;

function TaurusTLS_CustomX509VerifyParam.TPurposeHelper.IsEqualTo(
  Value: TIdC_Int): boolean;
begin
  Result:=Value = AsInt;
end;

{ TaurusTLS_CustomX509VerifyParam.THostCheckFlagHelper }

function TaurusTLS_CustomX509VerifyParam.THostCheckFlagHelper.GetAsInt: TIdC_UINT;
begin
  Result:=ToInt(Self);
end;

procedure TaurusTLS_CustomX509VerifyParam.THostCheckFlagHelper.SetAsInt(
  Value: TIdC_UINT);
begin
  Self:=FromInt(Value);
end;

class function TaurusTLS_CustomX509VerifyParam.THostCheckFlagHelper.FromInt(
  Value: TIdC_UINT): THostCheckFlag;
var
  i: TIdC_UINT;

begin
  // check if AVal in range of OpenSSL X509_CHECK_FLAG_* constants
  // and a single bit is set.
  if (Value = 0) or ((Value and (Value-1)) <> 0)
    or ((Value or cX509hckMask) <> cX509hckMask) then
    raise EInvalidCast.Create('Invalid X509 Host Check Verify Flag.');
  i:=Ord(Low(THostCheckFlag));
  while (1 shl i) < Value do
    Inc(i);
{$I RangeCheck-OFF.inc}
  Result:=THostCheckFlag(i);
{$I RangeCheck-ON.inc}
end;

class function TaurusTLS_CustomX509VerifyParam.THostCheckFlagHelper.ToInt(
  Value: THostCheckFlag): TIdC_UINT;
begin
  Result:=TIdC_UINT(1 shl Ord(Value));
end;

function TaurusTLS_CustomX509VerifyParam.THostCheckFlagHelper.IsEqualTo(
  Value: TIdC_UINT): boolean;
begin
  Result:=Value = AsInt;
end;

{ TaurusTLS_CustomX509VerifyParam.THostCheckFlagsHelper }

function TaurusTLS_CustomX509VerifyParam.THostCheckFlagsHelper.GetAsInt: TIdC_UINT;
begin
  Result:=ToInt(Self);
end;

procedure TaurusTLS_CustomX509VerifyParam.THostCheckFlagsHelper.SetAsInt(
  Value: TIdC_UINT);
begin
  Self:=FromInt(Value);
end;

class function TaurusTLS_CustomX509VerifyParam.THostCheckFlagsHelper.FromInt(
  Value: TIdC_UINT): THostCheckFlags;
begin
  // check if AVal in range of OpenSSL X509_CHECK_FLAG_* constants
  // and a single bit is set.
  if ((Value or cX509hckMask) <> cX509hckMask) then
    raise EInvalidCast.Create('Invalid X509 Host Check Verify Flags.');
{$I RangeCheck-OFF.inc}
  Result:=THostCheckFlags((@Value)^);
{$I RangeCheck-ON.inc}
end;

class function TaurusTLS_CustomX509VerifyParam.THostCheckFlagsHelper.ToInt(
  Value: THostCheckFlags): TIdC_UINT;
begin
  Result:=(TIdC_UINT((@Value)^) and cX509hckMask);
end;

function TaurusTLS_CustomX509VerifyParam.THostCheckFlagsHelper.IsEqualTo(
  Value: TIdC_UINT): boolean;
begin
  Result:=Value = AsInt;
end;

{ TaurusTLS_CustomX509VerifyParam }

constructor TaurusTLS_CustomX509VerifyParam.Create(AParam: PX509_VERIFY_PARAM);
begin
  if not Assigned(AParam) then
    DoException('PX509_VERIFY_PARAM is nil');
  inherited Create;
  FParam:=AParam;
end;

procedure TaurusTLS_CustomX509VerifyParam.DoException(AMessage: string);
begin
  raise ETaurusTLSX509StoreError.Create(AMessage);
end;

function TaurusTLS_CustomX509VerifyParam.GetVerifyFlags: TVerifyFlags;
begin
  Result:=TVerifyFlags.FromInt(X509_VERIFY_PARAM_get_flags(FParam));
end;

procedure TaurusTLS_CustomX509VerifyParam.SetVerifyFlags(const Value: TVerifyFlags);
var
  lFlags, lClearFlags: TVerifyFlags;

begin
  lFlags:=VerifyFlags;
  if X509_VERIFY_PARAM_set_flags(FParam, Value.AsInt) <> 1 then
    DoException('Unable to set X509_VERIFY_PARAM flags');
  lClearFlags:=lFlags-Value;
  if lClearFlags <> [] then
    if X509_VERIFY_PARAM_clear_flags(FParam, lClearFlags.AsInt) <> 1 then
      DoException('Unable to set X509_VERIFY_PARAM flags');
end;

function TaurusTLS_CustomX509VerifyParam.GetInheritanceFlags: TInheritanceFlags;
begin
  Result:=TInheritanceFlags.FromInt(X509_VERIFY_PARAM_get_inh_flags(Fparam));
end;

procedure TaurusTLS_CustomX509VerifyParam.SetInheritanceFlags(
  const Value: TInheritanceFlags);
begin
  if X509_VERIFY_PARAM_set_inh_flags(FParam, Value.AsInt) <> 1 then
    DoException('Unable to set X509_VERIFY_PARAM Inheritance Flags');
end;

function TaurusTLS_CustomX509VerifyParam.GetDepht: TIdC_Int;
begin
  Result:=X509_VERIFY_PARAM_get_depth(FParam);
end;

procedure TaurusTLS_CustomX509VerifyParam.SetDepth(const Value: TIdC_Int);
begin
  X509_VERIFY_PARAM_set_depth(FParam, Value);
end;

function TaurusTLS_CustomX509VerifyParam.GetAuthLevel: TAuthLevel;
begin
  Result:=X509_VERIFY_PARAM_get_auth_level(FParam);
end;

procedure TaurusTLS_CustomX509VerifyParam.SetAuthLevel(const Value: TAuthLevel);
begin
  X509_VERIFY_PARAM_set_auth_level(FParam, Value);
end;

function TaurusTLS_CustomX509VerifyParam.GetTime: TDateTime;
begin
  Result:=UnixToDateTime(X509_VERIFY_PARAM_get_time(FParam), True);
end;

procedure TaurusTLS_CustomX509VerifyParam.SetTime(const Value: TDateTime);
begin
  X509_VERIFY_PARAM_set_time(FParam, DateTimeToUnix(Value, True));
end;

function TaurusTLS_CustomX509VerifyParam.GetHostCheckFlags: THostCheckFlags;
begin
  Result:=THostCheckFlags.FromInt(X509_VERIFY_PARAM_get_hostflags(FParam));
end;

procedure TaurusTLS_CustomX509VerifyParam.SetHostCheckFlags(
  const Value: THostCheckFlags);
begin
  X509_VERIFY_PARAM_set_hostflags(FParam, Value.AsInt);
end;

function TaurusTLS_CustomX509VerifyParam.GetHostRaw(
  ANumber: TIdC_Int): PIdAnsiChar;
begin
  Result:=X509_VERIFY_PARAM_get0_host(FParam, ANumber);
end;

function TaurusTLS_CustomX509VerifyParam.GetHostA(
  ANumber: TIdC_Int): RawByteString;
begin
  Result:=RawByteString(GetHostRaw(ANumber));
end;

function TaurusTLS_CustomX509VerifyParam.GetHostW(
  ANumber: TIdC_Int): UnicodeString;
begin
  Result:=UnicodeString(GetHostRaw(ANumber));
end;

procedure TaurusTLS_CustomX509VerifyParam.SetHostA(Value: RawByteString);
begin
  if X509_VERIFY_PARAM_set1_host(FParam, PIdAnsiChar(Value), 0) <> 1 then
    DoException('Unable to set HostName for certificate validation.')
end;

procedure TaurusTLS_CustomX509VerifyParam.SetHostW(Value: UnicodeString);
begin
  SetHostA(RawByteString(Value));
end;

procedure TaurusTLS_CustomX509VerifyParam.AddHostA(Value: RawByteString);
begin
  if Value = '' then
    Exit;
  if X509_VERIFY_PARAM_add1_host(FParam, PIdAnsiChar(Value), 0) <> 1 then
    DoException('Unable to add HostName for certificate validation.')
end;

procedure TaurusTLS_CustomX509VerifyParam.AddHostW(Value: UnicodeString);
begin
  AddHostA(RawByteString(Value));
end;

function TaurusTLS_CustomX509VerifyParam.GetPerNameA: RawByteString;
begin
  Result:=RawByteString(X509_VERIFY_PARAM_get0_peername(FParam));
end;

function TaurusTLS_CustomX509VerifyParam.GetPerNameW: UnicodeString;
begin
  Result:=UnicodeString(X509_VERIFY_PARAM_get0_peername(FParam));
end;

function TaurusTLS_CustomX509VerifyParam.GetEmailRaw: PIdAnsiChar;
begin
  Result:=X509_VERIFY_PARAM_get0_email(FParam);
end;

function TaurusTLS_CustomX509VerifyParam.GetEmailA: RawByteString;
begin
  Result:=RawByteString(GetEmailRaw);
end;

function TaurusTLS_CustomX509VerifyParam.GetEmailW: UnicodeString;
begin
  Result:=UnicodeString(GetEmailRaw);
end;

procedure TaurusTLS_CustomX509VerifyParam.SetEMailA(Value: RawByteString);
begin
  if X509_VERIFY_PARAM_set1_email(FParam, PIdAnsiChar(Value),
    Length(Value)) <> 1 then
    DoException('Unable to add E-Mail address for certificate validation.')
end;

procedure TaurusTLS_CustomX509VerifyParam.SetEMailW(Value: UnicodeString);
begin
  SetEMailA(RawByteString(Value));
end;

procedure TaurusTLS_CustomX509VerifyParam.SetIpAddress(Value: TIdIPAddress);
var
  lData: Pointer;
  lIpv4: UInt32;
  lSize: TIdC_SizeT;

begin
  lData:=nil;
  lSize:=0;
  case Value.AddrType of
  Id_IPv4:
    begin
      lIpv4:=Value.IPv4;
      lData:=@lIpv4;
      lSize:=SizeOf(lIpv4);
    end;
  Id_IPv6:
    begin
      lData:=@Value.IPv6;
      lSize:=SizeOf(Value.IPv6);
    end;
  end;
  if X509_VERIFY_PARAM_set1_ip(FParam, lData, lSize) <> 1 then
    DoException('Unable to add IP address for certificate validation.')
end;

procedure TaurusTLS_CustomX509VerifyParam.SetIpAddressA(Value: RawByteString);
begin
  if X509_VERIFY_PARAM_set1_ip_asc(FParam, PIdAnsiChar(Value)) <> 1 then
    DoException('Unable to add IP address for certificate validation.')
end;

procedure TaurusTLS_CustomX509VerifyParam.SetIpAddressW(Value: UnicodeString);
begin
  SetIpAddressA(RawByteString(Value));
end;

function TaurusTLS_CustomX509VerifyParam.GetIpAddressA: RawByteString;
var
  lResult: PIdAnsiChar;

begin
  lResult:=nil;
  try
    lResult:=X509_VERIFY_PARAM_get1_ip_asc(FParam);
    Result:=RawByteString(lResult);
  finally
    OPENSSL_free(lResult);
  end;
end;

function TaurusTLS_CustomX509VerifyParam.GetIpAddressW: UnicodeString;
var
  lResult: PIdAnsiChar;

begin
  lResult:=nil;
  try
    lResult:=X509_VERIFY_PARAM_get1_ip_asc(FParam);
    Result:=UnicodeString(lResult);
  finally
    OPENSSL_free(lResult);
  end;
end;

function TaurusTLS_CustomX509VerifyParam.GetPurpose: TPurpose;
begin
  Result:=TPurpose.FromInt(X509_VERIFY_PARAM_get_purpose(FParam));
end;

procedure TaurusTLS_CustomX509VerifyParam.SetPurpose(Value: TPurpose);
begin
  if X509_VERIFY_PARAM_set_purpose(FParam, Value.AsInt) <> 0 then
    DoException('Unable to set or change certificate validation purpose.')
end;

{ TTaurusTLS_OSSLStore.TStoreInfoHelper }

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
      FData.FPKey:=POSSL_STORE_INFO.CloneParams(AInfo);
    sitPubKey:
      FData.FPKey:=POSSL_STORE_INFO.ClonePubKey(AInfo);
    sitPrivKey:
      FData.FPKey:=POSSL_STORE_INFO.ClonePrivKey(AInfo);
    sitCert:
      FData.FCert:=POSSL_STORE_INFO.CloneCert(AInfo);
    sitCRL:
      FData.FCrl:=POSSL_STORE_INFO.CloneCrl(AInfo);
  end;
end;

destructor TTaurusTLS_OSSLStore.TStoreItem.Destroy;
begin
  case FData.FType of
    sitParams, sitPubKey, sitPrivKey:
      EVP_PKEY_free(FData.FPKey);
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

function TTaurusTLS_OSSLStore.TStoreItem.GetName: RawByteString;
begin
  Result:=FData.FName;
end;

function TTaurusTLS_OSSLStore.TStoreItem.GetParams: PEVP_PKEY;
begin
  if FData.FType = sitParams then
    Result:=FData.FPKey
  else
    Result:=nil;
end;

function TTaurusTLS_OSSLStore.TStoreItem.GetPubKey: PEVP_PKEY;
begin
  if FData.FType = sitPubKey then
    Result:=FData.FPKey
  else
    Result:=nil;
end;

function TTaurusTLS_OSSLStore.TStoreItem.GetPrivKey: PEVP_PKEY;
begin
  if FData.FType = sitPrivKey then
    Result:=FData.FPKey
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

{ TTaurusTLS_OSSLStore }

constructor TTaurusTLS_OSSLStore.Create(ACtx: POSSL_STORE_CTX;
  ALoadFilter: TStoreElements);
begin
  if not Assigned(ACtx) then
    DoException('OSSL Context was not initialized.');
  inherited Create;
  FList:=TListInfo.Create;
  DoLoad(ACtx, ALoadFilter);
  if OSSL_STORE_close(ACtx) <> 1 then
    DoException('Error closing OSSL_STORE');
end;

constructor TTaurusTLS_OSSLStore.Create(AUri: RawByteString;
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
end;

function TTaurusTLS_OSSLStore.GetCount(AType: TStoreElement): TIdC_Uint;
begin
  Result:=FCounters[AType];
end;

procedure TTaurusTLS_OSSLStore.DoException(AMessage: string);
begin
  ETaurusTLSOSSLStoreError.RaiseException(AMessage);
end;

procedure TTaurusTLS_OSSLStore.DoLoad(ACtx: POSSL_STORE_CTX;
  ALoadFilter: TStoreElements);
var
  lFilter: TStoreElements;
  lInfo: POSSL_STORE_INFO;
  lItem: TStoreItem;

begin
  lFilter:=ALoadFilter;
  while not POSSL_STORE_CTX.Eof(ACtx) do
  begin
    lInfo:=POSSL_STORE_CTX.Load(ACtx);
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

{ TTaurusTLS_OSSLStoreHelper.TEnumerator }

constructor TTaurusTLS_OSSLStoreHelper.TEnumerator.Create(AList: TListInfo;
  AFilter: TStoreElements);
begin
  FEnum:=AList.GetEnumerator;
  FFilter:=AFilter;
  FCurrent:=nil;
end;

destructor TTaurusTLS_OSSLStoreHelper.TEnumerator.Destroy;
begin
  FreeAndNil(FEnum);
  inherited;
end;

function TTaurusTLS_OSSLStoreHelper.TEnumerator.GetCurrent: TStoreItem;
begin
  Result:=FEnum.Current;
end;

function TTaurusTLS_OSSLStoreHelper.TEnumerator.GetEnumerator: TEnumerator;
begin
  Result:=Self;
end;

function TTaurusTLS_OSSLStoreHelper.TEnumerator.MoveNext: boolean;
begin
  Result:=False;
  while FEnum.MoveNext do
    if FEnum.Current.&Type in FFilter then
      Exit(True);
end;

{ TTaurusTLS_OSSLStoreHelper }

function TTaurusTLS_OSSLStoreHelper.GetEnumerator(
  const AFilter: TTaurusTLS_OSSLStore.TStoreElements): TEnumerator;
begin
  Result:=TEnumerator.Create(FList, AFilter);
end;

function TTaurusTLS_OSSLStoreHelper.GetEnumerator: TEnumerator;
begin
  Result:=GetEnumerator(cStoreAElementsAll);
end;

{ TaurusTLS_X509VerifyParam }

constructor TaurusTLS_X509VerifyParam.Create;
begin
  inherited Create(X509_VERIFY_PARAM_new);
end;

destructor TaurusTLS_X509VerifyParam.Destroy;
begin
  X509_VERIFY_PARAM_free(FParam);
  inherited;
end;

{ TaurusTLS_X509Store.TVfyParam }

constructor TaurusTLS_X509Store.TVfyParam.Create(AStore: PX509_STORE);
begin
  inherited Create(X509_STORE_get0_param(AStore));
end;

{ TaurusTLS_X509Store }

constructor TaurusTLS_X509Store.Create;
begin
  FStore:=X509_STORE_new;
  if not Assigned(FStore) then
    DoException('Unable to create X509_STORE instance.');
  inherited;
end;

constructor TaurusTLS_X509Store.Create(AStore: TTaurusTLS_OSSLStore;
  AFilter: TX509Elements);
begin
  Create;
  LoadFromStore(AStore, AFilter);
end;

destructor TaurusTLS_X509Store.Destroy;
begin
  X509_STORE_free(FStore);
  inherited;
end;

procedure TaurusTLS_X509Store.DoException(AMessage: string);
begin
  ETaurusTLSX509StoreError.RaiseException(AMessage);
end;

procedure TaurusTLS_X509Store.LoadFromStore(AStore: TTaurusTLS_OSSLStore;
  AFilter: TX509Elements);
var
  lElement: TTaurusTLS_OSSLStore.TStoreItem;

begin
  if not Assigned(AStore) then
    Exit;
  AFilter:=AFilter*cX509ElementsAll; //Only Certificates and CRLs can be added
  for lElement in AStore.GetEnumerator(AFilter) do
  begin
    case lElement.&Type of
    sitCert: AddCert(lElement.Cert);
    sitCrl:  AddCrl(lElement.GetCrl);
    end;
  end;
end;

procedure TaurusTLS_X509Store.AddCert(ACert: PX509);
begin
  if X509_STORE_add_cert(FStore, ACert) <> 1 then
    DoException('Unable to add certificate to the X509_STORE.')
end;

procedure TaurusTLS_X509Store.AddCrl(ACrl: PX509_CRL);
begin
  if X509_STORE_add_crl(FStore, ACrl) <> 1 then
    DoException('Unable to add CRL to the X509_STORE.')
end;

procedure TaurusTLS_X509Store.SetParam(
  AVfyParam: TaurusTLS_CustomX509VerifyParam);
begin
  if not Assigned(AVfyParam) then
    Exit;
  if X509_STORE_set1_param(FStore, AVfyParam.VfyParam) <> 1 then
    DoException('Unable to set X509_Verify_Params to X509_Store.');
  FVfyParam:=nil;
end;

function TaurusTLS_X509Store.GetParam: TaurusTLS_CustomX509VerifyParam;
begin
  if not Assigned(FVfyParam) then
    FVfyParam:=TVfyParam.Create(FStore);
  Result:=FVfyParam;
end;

end.
