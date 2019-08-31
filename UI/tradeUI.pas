unit tradeUI;

interface

uses
  Classes, Forms, Dialogs, StdCtrls, Graphics, SysUtils, helpers, DBXJSON,
  Controls, ExtCtrls, UIContainer, OleCtrls, SHDocVw, ActiveX;

type
  TTradeUI = class (TUIContainer)
  private
    FBrowser: TWebBrowser;
    procedure SetBrowser(const Value: TWebBrowser);
  published
    constructor Create (AOwner: TComponent); override;
    property Browser : TWebBrowser read FBrowser write SetBrowser;
  end;

implementation

{ TTradeUI }

constructor TTradeUI.Create(AOwner: TComponent);
var
  RS : TResourceStream;
begin
  inherited;
  // Create the browser
  Browser := TWebBrowser.Create(self);
  TControl(FBrowser).Parent := self;
  Browser.Align := alClient;
  Browser.Navigate('about:blank');

  // Wait till loaded
  while Browser.ReadyState < READYSTATE_INTERACTIVE do
   Application.ProcessMessages;

  // Load the HTML from Resource
  if Assigned(Browser.Document) then
  begin
    rs := TResourceStream.Create(hInstance, 'tradeHTML', 'HTML');
    try
      (Browser.Document as IPersistStreamInit).Load(TStreamAdapter.Create(rs));
    finally
      rs.Free;
    end;
  end;
end;

procedure TTradeUI.SetBrowser(const Value: TWebBrowser);
begin
  FBrowser := Value;
end;

end.