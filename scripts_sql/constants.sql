----------------------
-- SCALAR CONSTANTS --
----------------------

CREATE FUNCTION dbo.CONST_ACRE_TO_FT2() RETURNS INT AS
BEGIN 
	RETURN 43560
END;

CREATE FUNCTION dbo.CONST_M_TO_FT() RETURNS FLOAT AS
BEGIN
	RETURN 3.28084
END;

CREATE FUNCTION dbo.CONST_M2_TO_FT2() RETURNS FLOAT AS
BEGIN
	RETURN POWER(dbo.CONST_M_TO_FT(), 2)
END;

CREATE FUNCTION dbo.CONST_KG_TO_LBS() RETURNS FLOAT AS
AS BEGIN
	RETURN 2.2046226
END;

CREATE FUNCTION dbo.CONST_IDEAL_MOISTURE (@product VARCHAR(50))
  /**
   * Returns Ideal Moisture for adjusted yields for products
   * 
   * @param product - The first input number
   * @returns moisture percentage
   */
RETURNS REAL AS 
BEGIN
	DECLARE @IDEAL_MOISTURE REAL
	SET @IDEAL_MOISTURE = 
		CASE @product
			WHEN 'corn' THEN 15.5
			WHEN 'soybean' THEN 13
			WHEN 'barley' THEN 14.5
			WHEN 'wheat' THEN 13.5
			WHEN 'wheat grain' THEN 13.5
			WHEN 'wheat straw' THEN 13
			WHEN 'oat' THEN 14
			else NULL
		END;
RETURN @IDEAL_MOISTURE
END;

CREATE FUNCTION dbo.CONST_IDEAL_BUSHEL (@product VARCHAR(50))
  /**
   * Returns Ideal pounds per bushel for adjusted yields for products
   * 
   * @param product - The first input number
   * @returns ideal pounds per bushel
   */
RETURNS REAL
AS 
BEGIN
	DECLARE @IDEAL_BUSHEL REAL
	SET @IDEAL_BUSHEL = 
		CASE @product
			WHEN 'corn' THEN 56
			WHEN 'soybean' THEN 60
			WHEN 'barley' THEN 48
			WHEN 'wheat' THEN 60
			WHEN 'wheat grain' THEN 60
			WHEN 'wheat straw' THEN 60
			WHEN 'oat' THEN 32
			else 0
		END;
RETURN @IDEAL_BUSHEL
END;

