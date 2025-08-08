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


end.
