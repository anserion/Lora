//Copyright 2018 Andrey S. Ionisyan (anserion@gmail.com)
//
//Licensed under the Apache License, Version 2.0 (the "License");
//you may not use this file except in compliance with the License.
//You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
//Unless required by applicable law or agreed to in writing, software
//distributed under the License is distributed on an "AS IS" BASIS,
//WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//See the License for the specific language governing permissions and
//limitations under the License.

//учебный шаблон клиента идентификации с нулевым разглашением (доказывающий)
//по протоколу Фиата-Шамира

//Общая постановка задачи.
//Доказывающая сторона владеет секретом S (число)
//Проверяющая сторона должна убедиться в наличии S у доказывающей стороны в
//условиях отсутствия доступа непосредственно к S.
//Проверяющая сторона не имеет возможности доказать третьей стороне наличие S
//у доказывающей стороны

// протокол идентификации с нулевым разглашением Фиата-Шамира
// 1. Центр распределения ключей генерирует два простых числа p,q
//    которые сообщаются доказывающей стороне (секретно)
//    и их произведение n=p*q (публично).
//    (в данной программе p и q не генерируются, n полагается известным всем)
//    доп. условие, повышающее качество алгоритма: p,q сравнимы с 3 по модулю 4
//    криптостойкость алгоритма основана на сложности операции
//    извлечения корня квадратного в конечных полях
// 2. Доказывающая сторона генерирует и оглашает публичное свойство секрета:
//    V=(S*S) mod n
// 3. Доказывающая сторона генерирует произвольное (случайное) число R,
//    пригодное для дальнейшей работы алгоритма 0<r<n.
// 4. Доказывающая сторона публикует значение X=(R*R) mod n.
// 5. Проверяющая сторона генерирует и публикует случайный бит E.
// 6. Если E=0, то доказывающая сторона публикует значение Y=R,
//    если E=1, то доказывающая сторона публикует значение Y=(R*S) mod n.
// 7. Если E=0, то проверяющая сторона сравнивает X и (Y*Y) mod n на равенство
//    Если E=1, то проверяющая сторона сравнивает (X*V) mod n и (Y*Y) mod n на равенство
// 8. В случае неравенства протокол прекращает работу (срыв доказательства)
// 9. Шаги 3-8 повторяются t раз (достаточно 20-40 раз) для убеждения проверяющего.
//    Вероятность обмана проверяющего при этом равна 2^(-t), т.е. очень мала.

unit lora_key_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BTN_green_code: TButton;
    BTN_yellow_code: TButton;
    BTN_red_code: TButton;
    BTN_reconfigure: TButton;
    CB_show_secret: TCheckBox;
    Edit_p_secret: TEdit;
    Edit_q_secret: TEdit;
    Edit_s_secret: TEdit;
    Edit_key_id: TEdit;
    Label1: TLabel;
    Memo1: TMemo;
    procedure BTN_green_codeClick(Sender: TObject);
    procedure BTN_reconfigureClick(Sender: TObject);
    procedure BTN_red_codeClick(Sender: TObject);
    procedure BTN_yellow_codeClick(Sender: TObject);
    procedure CB_show_secretChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  P,Q,N,R,S,V,X:integer;
  SV,SN:string;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  P:=7; Q:=11; N:=P*Q;
  S:=13; V:=(S*S) mod N;

  SN:=IntToStr(N);
  if N<1000 then SN:='0'+IntToStr(N);
  if N<100 then SN:='00'+IntToStr(N);
  if N<10 then SN:='000'+IntToStr(N);

  SV:=IntToStr(V);
  if V<1000 then SV:='0'+IntToStr(V);
  if V<100 then SV:='00'+IntToStr(V);
  if V<10 then SV:='000'+IntToStr(V);

  Edit_key_id.text:=SN+SV;
end;

procedure TForm1.CB_show_secretChange(Sender: TObject);
begin
  if CB_show_secret.Checked then
  begin
    Edit_p_secret.text:=IntToStr(P);
    Edit_q_secret.text:=IntToStr(Q);
    Edit_s_secret.text:=IntToStr(S);
  end else
  begin
    Edit_p_secret.text:='******';
    Edit_q_secret.text:='******';
    Edit_s_secret.text:='******';
  end;
end;

procedure TForm1.BTN_reconfigureClick(Sender: TObject);
begin
  if CB_show_secret.Checked then
  begin
    P:=StrToInt(Edit_p_secret.text);
    Q:=StrToInt(Edit_q_secret.text);
    S:=StrToInt(Edit_s_secret.text);
    N:=P*Q;
    V:=(S*S) mod N;

    SN:=IntToStr(N);
    if N<1000 then SN:='0'+IntToStr(N);
    if N<100 then SN:='00'+IntToStr(N);
    if N<10 then SN:='000'+IntToStr(N);

    SV:=IntToStr(V);
    if V<1000 then SV:='0'+IntToStr(V);
    if V<100 then SV:='00'+IntToStr(V);
    if V<10 then SV:='000'+IntToStr(V);

    Edit_key_id.text:=SN+SV;
  end;
end;

procedure TForm1.BTN_red_codeClick(Sender: TObject);
begin
  BTN_green_code.caption:='Зеленый код';
  BTN_yellow_code.caption:='Желтый код';
  BTN_red_code.caption:=IntToStr((R*S)mod N);
end;

procedure TForm1.BTN_yellow_codeClick(Sender: TObject);
begin
  BTN_green_code.caption:='Зеленый код';
  BTN_yellow_code.caption:=IntToStr(R);
  BTN_red_code.caption:='Красный код';
end;

procedure TForm1.BTN_green_codeClick(Sender: TObject);
begin
  randomize;
  R:=random(N-1)+1;
  X:=(R*R) mod N;
  BTN_green_code.caption:=IntToStr(X);
  BTN_yellow_code.caption:='Желтый код';
  BTN_red_code.caption:='Красный код';
end;

end.

