CREATE TABLE protect_category
(
  protect_category_code integer NOT NULL,
  protect_category_rus character varying,
  CONSTRAINT protect_category_pkey PRIMARY KEY (protect_category_code)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE protect_category
  OWNER TO postgres;
  
Insert into protect_category (protect_category_code, protect_category_rus) 
values (100, '����, ������������� �� ����� ���������� ��������� �����������');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (120, '����, ������������� � ������������ �����');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (130, '����, ����������� ������� ������ ��������� � ���� ��������:');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (131, '����, ������������� � ������ � ������ ������ ��� ���������� ������ ���������� ��������� � ������������-�������� �������������');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (132, '�������� ������ ����� ����� ��������������� ����� ������ �����������, ����������� ������������� ����� ������ ����������� � ������������� ����� ������ �����������, ����������� � ������������� ��������� ���������� ���������');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (133, '������� ����');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (134, '������������ ����');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (135, '��������� ����');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (139, '����, ������������� � ������, ������ � ������� ��� ������� ���������� (�����-����������) ������ �������-��������������� ���������� � ��������');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (140, '������ ����:');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (141, '��������������� �������� ������ ������');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (142, '����������������� ����');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (143, '����, ������������� � ���������, �������������, �����������, ����-��������� �����, ������, �����');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (144, '����, ������� ������� ��� ������������ ��������');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (145, '�������-����������� ����');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (146, '������ �������� ����������');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (147, '��������� ����');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (148, '��������� ������ �����, ������������� ����� ������ ��������');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (149, '��������������� ������ �����');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (150, '����� �������� ������� �����');
