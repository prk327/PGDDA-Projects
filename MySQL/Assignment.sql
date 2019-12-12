-- SQL Assignment

use assignment;
SET SQL_SAFE_UPDATES = 0;

--We need to create a table named 'Bajaj_Auto' containing the date, close price, 20 Day MA and 50 Day MA. 

CREATE TABLE bajaj1 AS SELECT Date, `Close Price`, 
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 19 PRECEDING) AS "20 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 49 PRECEDING) AS "50 Day MA",
FROM bajaj_auto;

-- we need to Create a new table named 'Eicher_Motors' 

CREATE TABLE eicher_motors1 AS SELECT Date, `Close Price`, 
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 19 PRECEDING) AS "20 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 49 PRECEDING) AS "50 Day MA",
FROM eicher_motors;

-- we need to Create a new table named 'Hero'

CREATE TABLE hero1 AS SELECT Date, `Close Price`, 
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 19 PRECEDING) AS "20 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 49 PRECEDING) AS "50 Day MA",
FROM hero;

-- we need to Create a new table named 'TVS_Motors'

CREATE TABLE tvs_motors1 AS SELECT Date, `Close Price`, 
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 19 PRECEDING) AS "20 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 49 PRECEDING) AS "50 Day MA",
FROM tvs_motors;

-- we need to Create a new table named 'Infosys'

CREATE TABLE infosys1 AS SELECT Date, `Close Price`, 
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 19 PRECEDING) AS "20 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 49 PRECEDING) AS "50 Day MA",
FROM infosys;


-- we need to Create a new table named 'TCS' 

CREATE TABLE tcs1 AS SELECT Date, `Close Price`, 
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 19 PRECEDING) AS "20 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 49 PRECEDING) AS "50 Day MA",
FROM tcs;

-- we need to Create a master table containing the date and close price of all the six stocks.

select A.Date, A.`Close Price` AS "Bajaj",
			 B.`Close Price` AS "TCS",
			 C.`Close Price` AS "TVS",
			 D.`Close Price` AS "Infosys",
			 E.`Close Price` AS "Eicher",
			 F.`Close Price` AS "Hero"
FROM bajaj_auto A
LEFT OUTER JOIN tcs B ON A.Date = B.Date
LEFT OUTER JOIN tvs_motors C ON A.Date = C.Date
LEFT OUTER JOIN infosys D ON A.Date = D.Date
LEFT OUTER JOIN eicher_motors E ON A.Date = E.Date
LEFT OUTER JOIN hero F ON A.Date = F.Date
ORDER BY A.Date;

-- now we need to created a UDF to generate buy and sell signal and Store in table named 'bajaj2'

--below is the function Trading that will return buy, sell and hold base on the criteria given

drop function if exists Trading;

DELIMITER $$

create function Trading(MA19 float(8,2), MA49 float(8,2), MA20 float(8,2), MA50 float(8,2))
  returns Varchar(20) 
  deterministic
begin
  declare VarTrad Varchar(20);

   if (MA49 > MA19 and MA20 >= MA50) then
     set VarTrad = "Buy";
   elseif (MA19 > MA49 and MA50 >= MA20) then
     set VarTrad = "Sell";
   else
     set VarTrad = "Hold";
  end if;

return (VarTrad);
end
$$

DELIMITER ;

--we will create 6 different table with signal column

CREATE TABLE bajaj2 AS SELECT Date, `Close Price`,
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 18 PRECEDING) AS "19 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 48 PRECEDING) AS "49 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 19 PRECEDING) AS "20 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 49 PRECEDING) AS "50 Day MA"
FROM bajaj_auto;

--we need to add a blank column with name signals

alter table bajaj2
ADD COLUMN Signals nvarchar(100) null;  

-- now we will use the function to update the column with signal value

Update bajaj2 SET Signals=Trading(`19 Day MA`, `49 Day MA`, `20 Day MA`, `50 Day MA`);

-- now we will remove the unnecessary columns

ALTER TABLE bajaj2
DROP COLUMN `19 Day MA`,
DROP COLUMN `49 Day MA`,
DROP COLUMN `20 Day MA`,
DROP COLUMN `50 Day MA`
;

--we will create table with signal column

create TABLE eicher_motors2 AS SELECT Date, `Close Price`,
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 18 PRECEDING) AS "19 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 48 PRECEDING) AS "49 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 19 PRECEDING) AS "20 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 49 PRECEDING) AS "50 Day MA"
        from eicher_motors;

--we need to add a blank column with name signals

alter table eicher_motors2
ADD COLUMN Signals nvarchar(100) null

-- now we will use the function to update the column with signal value

Update eicher_motors3 SET Signals=Trading(`19 Day MA`, `49 Day MA`, `20 Day MA`, `50 Day MA`);

-- now we will remove the unnecessary columns
ALTER TABLE eicher_motors2
DROP COLUMN `19 Day MA`,
DROP COLUMN `49 Day MA`,
DROP COLUMN `20 Day MA`,
DROP COLUMN `50 Day MA`
;

--we will create table with signal column

CREATE TABLE hero2 AS SELECT Date, `Close Price`,
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 18 PRECEDING) AS "19 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 48 PRECEDING) AS "49 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 19 PRECEDING) AS "20 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 49 PRECEDING) AS "50 Day MA"

FROM hero;

--we need to add a blank column with name signals

alter table hero2
ADD COLUMN Signals nvarchar(100) null     ; 

-- now we will use the function to update the column with signal value

Update hero2 SET Signals=Trading(`19 Day MA`, `49 Day MA`, `20 Day MA`, `50 Day MA`);

-- now we will remove the unnecessary columns

ALTER TABLE hero2
DROP COLUMN `19 Day MA`,
DROP COLUMN `49 Day MA`,
DROP COLUMN `20 Day MA`,
DROP COLUMN `50 Day MA`

;



CREATE TABLE tvs_motors2 AS SELECT Date, `Close Price`,
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 18 PRECEDING) AS "19 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 48 PRECEDING) AS "49 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 19 PRECEDING) AS "20 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 49 PRECEDING) AS "50 Day MA"
		
FROM tvs_motors

alter table tvs_motors2
ADD COLUMN Signals nvarchar(100) null; 

Update tvs_motors2 SET Signals=Trading(`19 Day MA`, `49 Day MA`, `20 Day MA`, `50 Day MA`);

ALTER TABLE tvs_motors2
DROP COLUMN `19 Day MA`,
DROP COLUMN `49 Day MA`,
DROP COLUMN `20 Day MA`,
DROP COLUMN `50 Day MA`

;

--Now we create a table for signal

CREATE TABLE infosys2 AS SELECT Date, `Close Price`,
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 18 PRECEDING) AS "19 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 48 PRECEDING) AS "49 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 19 PRECEDING) AS "20 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 49 PRECEDING) AS "50 Day MA"
		
FROM infosys;

alter table infosys2
ADD COLUMN Signals nvarchar(100) null; 

Update infosys2 SET Signals=Trading(`19 Day MA`, `49 Day MA`, `20 Day MA`, `50 Day MA`);

ALTER TABLE infosys2
DROP COLUMN `19 Day MA`,
DROP COLUMN `49 Day MA`,
DROP COLUMN `20 Day MA`,
DROP COLUMN `50 Day MA`

;

--Now we create a table for signal

CREATE TABLE tcs2 AS SELECT Date, `Close Price`,
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 18 PRECEDING) AS "19 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 48 PRECEDING) AS "49 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 19 PRECEDING) AS "20 Day MA",
		AVG(`Close Price`) OVER (ORDER BY Date ASC ROWS 49 PRECEDING) AS "50 Day MA"
FROM tcs;

alter table tcs2
ADD COLUMN Signals nvarchar(100) null; 

Update tcs2 SET Signals=Trading(`19 Day MA`, `49 Day MA`, `20 Day MA`, `50 Day MA`);


ALTER TABLE tcs2
DROP COLUMN `19 Day MA`,
DROP COLUMN `49 Day MA`,
DROP COLUMN `20 Day MA`,
DROP COLUMN `50 Day MA`

;

--will creata a soreprocedure to get the bajar buy , seel and hold value

drop procedure if exists TradingBajaj;

DELIMITER $$
create procedure TradingBajaj
  (in n date)
begin
  select Signals
  from bajaj2
  where 
    Date = n;
end $$
DELIMITER ;
