unit tradeUI;

interface

uses
  Windows, Classes, Forms, Dialogs, StdCtrls, Graphics, SysUtils, helpers,
  DBXJSON, Controls, ExtCtrls, UIContainer, Messages, Variants,
  // CEF LIBRARIES
  uCEFChromium, uCEFWindowParent, uCEFChromiumWindow, uCEFTypes, uCEFInterfaces,
  uCEFWinControl, uCEFApplication;

type
  TTradeUI = class (TUIContainer)
  private
    FChromiumWindow: TChromiumWindow;
    FUserID: string;

    // Handle TChromiumWindow events
    procedure ChromiumWindowAfterCreated(Sender: TObject);
    procedure ChromiumWindowClose(Sender: TObject);
    procedure ChromiumWindowBeforeClose(Sender: TObject);
    procedure HandleLoadStart(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);

    // HANDLE THE MESSAGES FOR CHROMIUM
    // You have to handle this two messages to call NotifyMoveOrResizeStarted or some page elements will be misaligned.
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    // You also have to handle these two messages to set GlobalCEFApp.OsmodalLoop
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
    procedure SetUserID(const Value: string);
  protected
    procedure Chromium_OnBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
  published
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    property UserID : string read FUserID write SetUserID;
  end;

implementation

{ TTradeUI }

constructor TTradeUI.Create(AOwner: TComponent);
begin
  inherited;
  FChromiumWindow := TChromiumWindow.Create(self);
  FChromiumWindow.OnAfterCreated := ChromiumWindowAfterCreated;
  FChromiumWindow.OnBeforeClose := ChromiumWindowBeforeClose;
  FChromiumWindow.OnClose := ChromiumWindowClose;
  FChromiumWindow.Parent := self;
  FChromiumWindow.Align := alClient;
  FChromiumWindow.ChromiumBrowser.OnLoadStart := HandleLoadStart;
  UserID := '';
end;

destructor TTradeUI.Destroy;
begin
  FChromiumWindow.CloseBrowser(True);
  inherited;
end;

procedure TTradeUI.HandleLoadStart(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; transitionType: TCefTransitionType);
begin
  // Inject the username variable into the browser view
  FChromiumWindow.ChromiumBrowser.Browser.MainFrame.ExecuteJavaScript(
    '__inject = { client: "PASCAL", client_id: "' + UserID + '" }',
    'http://localhost:1234', 0);
end;

procedure TTradeUI.Init;
begin
  // For simplicity, blocks all popup windows and new tabs
  FChromiumWindow.ChromiumBrowser.OnBeforePopup := Chromium_OnBeforePopup;

  // You *MUST* call CreateBrowser to create and initialize the browser.
  // This will trigger the AfterCreated event when the browser is fully
  // initialized and ready to receive commands.

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  while not(FChromiumWindow.CreateBrowser) and not(FChromiumWindow.Initialized) do
    Application.ProcessMessages;
end;

procedure TTradeUI.SetUserID(const Value: string);
begin
  FUserID := Value;
end;

procedure TTradeUI.ChromiumWindowAfterCreated(Sender: TObject);
begin
  FChromiumWindow.LoadURL('http://localhost:1234/');
end;

procedure TTradeUI.ChromiumWindowBeforeClose(Sender: TObject);
begin
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TTradeUI.ChromiumWindowClose(Sender: TObject);
begin
  if not(FChromiumWindow.DestroyChildWindow) then
    begin
      PostMessage(Handle, WM_CLOSE, 0, 0);
    end;
end;

procedure TTradeUI.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (FChromiumWindow <> nil) then FChromiumWindow.NotifyMoveOrResizeStarted;
end;

procedure TTradeUI.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (FChromiumWindow <> nil) then FChromiumWindow.NotifyMoveOrResizeStarted;
end;

procedure TTradeUI.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TTradeUI.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

procedure TTradeUI.Chromium_OnBeforePopup(      Sender             : TObject;
                                        const browser            : ICefBrowser;
                                        const frame              : ICefFrame;
                                        const targetUrl          : ustring;
                                        const targetFrameName    : ustring;
                                              targetDisposition  : TCefWindowOpenDisposition;
                                              userGesture        : Boolean;
                                        const popupFeatures      : TCefPopupFeatures;
                                        var   windowInfo         : TCefWindowInfo;
                                        var   client             : ICefClient;
                                        var   settings           : TCefBrowserSettings;
                                        var   extra_info         : ICefDictionaryValue;
                                        var   noJavascriptAccess : Boolean;
                                        var   Result             : Boolean);
begin
  // For simplicity, blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

end.