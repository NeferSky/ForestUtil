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
values (100, 'Леса, расположенные на особо охраняемых природных территориях');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (120, 'Леса, расположенные в водоохранных зонах');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (130, 'Леса, выполняющие функции защиты природных и иных объектов:');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (131, 'Леса, расположенные в первом и втором поясах зон санитарной охраны источников питьевого и хозяйственно-бытового водоснабжения');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (132, 'Защитные полосы лесов вдоль железнодорожных путей общего пользования, федеральных автомобильных дорог общего пользования и автомобильных дорог общего пользования, находящихся в собственности субъектов Российской Федерации');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (133, 'Зеленые зоны');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (134, 'Лесопарковые зоны');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (135, 'Городские леса');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (139, 'Леса, расположенные в первой, второй и третьей зон округов санитарной (горно-санитарной) охраны лечебно-оздоровительных местностей и курортов');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (140, 'Ценные леса:');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (141, 'Государственные защитные лесные полосы');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (142, 'Противоэрозионные леса');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (143, 'Леса, расположенные в пустынных, полупустынных, лесостепных, лесо-тундровых зонах, степях, горах');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (144, 'Леса, имеющие научное или исторические значение');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (145, 'Орехово-промысловые зоны');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (146, 'Лесные плодовые насаждения');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (147, 'Ленточные боры');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (148, 'Запретные полосы лесов, расположенные вдоль водных объектов');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (149, 'Нерестоохранные полосы лесов');
Insert into protect_category (protect_category_code, protect_category_rus) 
values (150, 'Особо защитные участки лесов');
