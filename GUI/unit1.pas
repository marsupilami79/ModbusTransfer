unit Unit1;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, SnapMB,
  ZConnection, ZXmlCompat, ModbusTransfer;

type

  { TForm1 }

  TForm1 = class(TForm)
    ConfigOnlyCB: TCheckBox;
    RunBtn: TButton;
    M: TMemo;
    ZConnection1: TZConnection;
    procedure RunBtnClick(Sender: TObject);
  private
    Dev: TSnapMBBroker;
    Registers: Array[0..9] of WORD;
    Config: TMbtConfig;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure Log(Message: String);
begin
  Form1.M.Lines.Append(FormatDateTime('YYYY-MM-DD HH:NN:SS', Now) + ' ' + Message);
end;

procedure TForm1.RunBtnClick(Sender: TObject);
begin
  if not Assigned(config) then begin
    Log('Loading configuration');
    Config := TMbtConfig.Create;
    Config.LogProcedure := Log;
    Config.LoadFromFile(ParamStr(0) + '.xml');
  end;
  if not ConfigOnlyCB.Checked then begin
    Log('Executing configuration');
    Config.Execute;
  end else begin
    Log('Not executing configuration.');
  end;
  M.Lines.Add('--> Done');
end;

end.

