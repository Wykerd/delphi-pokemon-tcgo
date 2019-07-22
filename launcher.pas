unit launcher;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, server, ComCtrls, client;

type
  TForm1 = class(TForm)
    btnStart: TButton;
    btnJoin: TButton;
    redDebug: TRichEdit;
    redClient: TRichEdit;
    procedure btnStartClick(Sender: TObject);
    procedure btnJoinClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Server: TServer;
  Client: TClient;

implementation

{$R *.dfm}

procedure TForm1.btnJoinClick(Sender: TObject);
begin
  Client := TClient.Create(Self);
  Client.Debug := redClient;
  Client.Start('localhost', 8080);
  Client.IOHandler.WriteLn('Client Test');
  Server.Broadcast('Broadcast Test');
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  TButton(Sender).Enabled := false;
  Server := TServer.Create(Self);
  Server.Debug := redDebug;
  Server.Start(8080);
end;

end.
