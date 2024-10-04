{
  This file is part of the Indy (Internet Direct) project, and is offered
  under the dual-licensing agreement described on the Indy website.
  (http://www.indyproject.org/)

  Copyright:
   (c) 1993-2024, Chad Z. Hower and the Indy Pit Crew. All rights reserved.
}

unit IdRegisterTaurusTLS;


interface

{$i IdCompilerDefines.inc}

uses
  Classes,
  {$IFDEF DOTNET}
  Borland.Vcl.Design.DesignIntF,
  Borland.Vcl.Design.DesignEditors
  {$ELSE}
    {$IFDEF FPC}
  PropEdits,
  ComponentEditors
    {$ELSE}
      {$IFDEF VCL_6_OR_ABOVE}
  DesignIntf,
  DesignEditors
      {$ELSE}
  Dsgnintf
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  ;

{$IFDEF HAS_TSelectionEditor}
type
  TTaurusTLSSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;
{$ENDIF}

procedure Register;

implementation

uses
  IdDsnCoreResourceStrings, // for RSRegIndyIOHandlers in dclIndyCore package
  {$IFDEF FPC}
  IdDsnResourceStrings,     // for RSProt in dclIndyProtocols package
  LResources,
  {$ENDIF}
  IdSSLTaurusTLS;

{$IFNDEF FPC}
//  {$R IdRegisterTaurusTLS.dcr}
{$ENDIF}

{$IFDEF HAS_TSelectionEditor}

{TTaurusTLSSelectionEditor}

procedure TTaurusTLSSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);
  //for new callback event
  Proc('IdCTypes'); {Do not localize}
  Proc('TaurusTLSHeaders_ossl_typ'); {Do not localize}
  Proc('IdSSLTaurusTLS_X509');
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents(RSRegIndyIOHandlers{$IFDEF FPC}+RSProt{$ENDIF}, [
    TIdServerIOHandlerSSLTaurusTLS,
    TIdSSLIOHandlerSocketTaurusTLS
  ]);

  {$IFDEF HAS_TSelectionEditor}
  RegisterSelectionEditor(TIdServerIOHandlerSSLTaurusTLS, TTaurusTLSSelectionEditor);
  RegisterSelectionEditor(TIdSSLIOHandlerSocketTaurusTLS, TTaurusTLSSelectionEditor);
  {$ENDIF}
end;

{$IFDEF FPC}
initialization
{$i IdRegisterTaurusTLS.lrs}
{$ENDIF}

end.
