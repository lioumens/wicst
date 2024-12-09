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

--drop function dbo.CONST_IDEAL_MOISTURE;
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
			WHEN 'wheat' THEN 13.5 -- combine into wheat grain
			WHEN 'wheat grain' THEN 13.5
			WHEN 'wheat straw' THEN 13
			WHEN 'oat grain' THEN 14
			else NULL
		END
RETURN @IDEAL_MOISTURE
END;


--drop function dbo.CONST_IDEAL_BUSHEL;
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
	SET
	@IDEAL_BUSHEL = 
		CASE @product
		WHEN 'corn' THEN 56
		WHEN 'soybean' THEN 60
		WHEN 'barley' THEN 48
		WHEN 'wheat' THEN 60
		WHEN 'wheat grain' THEN 60
		WHEN 'wheat straw' THEN 60
		WHEN 'oat grain' THEN 32
		else NULL
	END;

RETURN @IDEAL_BUSHEL
END;

-----------------------
-- HELPER FUNCTIONS  --
-----------------------

--drop function dbo.seq;
CREATE FUNCTION dbo.SEQ (
/*
 * Convenience function to generate sequence of numbers. Written as TVF.
 * 
 * Examples
 * select * from dbo.SEQ(10, DEFAULT, DEFAULT); -- return with default values
 * select * from dbo.SEQ(200, .5, 150); -- call with end, start, by specified
 */
	@end REAL,
    @start REAL = 1,
    @by REAL = 1
)
RETURNS @Nums TABLE (num REAL)
AS
BEGIN
	DECLARE @current REAL = @start;
	
	WHILE @current <= @end
	BEGIN
		INSERT INTO @Nums (num)
		VALUES (@current);
	
		SET @current = @current + @by;
	END
    RETURN;
END;



