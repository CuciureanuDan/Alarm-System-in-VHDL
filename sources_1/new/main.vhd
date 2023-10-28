----------------------------------------------------------------------------------
-- Company: UTCN
--future Engineer: Cuciureanu Dan
-- 
-- Create Date: 05/03/2023 10:20:01 PM
-- Design Name: 
-- Module Name: main - Behavioral
-- Project Name: Alarm System
-- Target Devices: Basys 3 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------

--READ ME

-- Codul functioneaza perfect mai putin partea de timer. Desi clk1hz functioneaza, ledul 15 clipeste bine, flagEnd nu se schimba cand timer ajunge la 15
-- am dorit si o implementare pe display de secunde, dar semnalul secunde nu se schimba, tot in acel numarator. Poate e o problema la numarator? nu am reusit sa o rezolv

--Cum functioneaza ?  prima data se apasa butonul de sus , sa porneasca timerul apoi se introduce codul cu cele 3 butoane, iar dupa ce led-ul 2 sau 3 e aprins functioneza
-- butonul de jos , care e resetul si se ia procesul de la inceput.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity main is

    Port ( rst   : in STD_LOGIC;                      --reset button: cel de jos
           clk   : in STD_LOGIC;                      -- trebuie pentru ceas
           
           start : in STD_LOGIC;                      -- butonul de start proiect: cel de sus
           btnL  : in STD_LOGIC;                      --butoane de introdus codul
           btnC  : in STD_LOGIC;
           btnR  : in STD_LOGIC;
           led15 : out std_logic_vector (15 downto 15); -- ledul care clipeste in secunde 
           led   : out STD_LOGIC_VECTOR (3 downto 0); -- ledurile pt alarma activa, time_start, cod corect, si alarma declansata

           an : out STD_LOGIC_VECTOR (3 downto 0); 
           seg : out STD_LOGIC_VECTOR (0 to 6); 
           dp : out STD_LOGIC);


end main;



architecture Behavioral of main is


component deBounce is
    port(   clk : in std_logic;
            rst : in std_logic;
            button_in : in std_logic;
            pulse_out : out std_logic
        );
    end component;

signal flagEnd: std_logic := '0';    -- conditia de verificare 
signal btnLd, btnRd,btnCd,startd : std_logic;

type state_type is (idle,countdown,verification); 

signal state    : state_type:= idle;


constant cod: integer:= 123; -- codul de acces
signal codIntrodus: integer:=0; -- codul introdus de utilizator

constant n : integer := 10**8; -- frecventa placutei
signal clk1hz : std_logic;
signal rst1: std_logic ;

signal secunde : integer:=15;  -- ar trebui aici sa incarce nr de secunde, e initializat cu 15 deoarece nu se modifica pe frecventa de 1s
--signal timer1     : integer;              -- a fost facut variabila in prescalor, aveam si un timer, a ramas in caz ca vreau sa modific


signal score : std_logic_vector (15 downto 0); -- ne ajuta la afisarea pe display

component driver7seg is
    Port ( clk : in STD_LOGIC; --100MHz board clock input
           Din : in STD_LOGIC_VECTOR (15 downto 0); --16 bit binary data for 4 displays
           an : out STD_LOGIC_VECTOR (3 downto 0); --anode outputs selecting individual displays 3 to 0
           seg : out STD_LOGIC_VECTOR (0 to 6); -- cathode outputs for selecting LED-s in each display
           dp_in : in STD_LOGIC_VECTOR (3 downto 0); --decimal point input values
           dp_out : out STD_LOGIC; --selected decimal point sent to cathodes
           rst : in STD_LOGIC); --global reset
end component driver7seg;



begin
deb1 : deBounce port map (clk => clk, rst => '0', button_in => btnL, pulse_out => btnLd);
deb2 : deBounce port map (clk => clk, rst => '0', button_in => btnR, pulse_out => btnRd);
deb3 : deBounce port map (clk => clk, rst => '0', button_in => btnC, pulse_out => btnCd);
deb4 : deBounce port map (clk => clk, rst => '0', button_in => start, pulse_out => startd);

PRESCALOR: process (clk,rst1) -- am adaugat timer1 in sensitivity list// comentariu vechi, lasat in caz ca vreau sa modific
variable timer1 : integer := 0;
begin
 
  if rst1 = '1' then    
    clk1hz <= '0'; 
  elsif rising_edge(clk) then
    if timer1 = n/2 - 1 then
      timer1:=0;
      clk1hz <= not clk1hz; 
    else
      clk1hz <= clk1hz;
      timer1:=timer1+1;
      
    end if;  
  end if;
  end process; 

helper : process(clk1hz,rst,flagEnd,secunde)  -- am adaugat timer in sensitivity list//comentariu vechi in caz ca vreau sa modific
variable timer : integer := 0;
begin

   if rst = '1' then 
       timer :=0; 
       if rising_edge(clk1hz) then
        if timer=15 then
            timer :=0;
            flagEnd<='1';
            secunde<=0;
        else
           timer:=timer+1;
           secunde<=secunde+1;
          -- flagEnd<='0';   -- l-am initializat cu 0 si nu mai e nevoie , se face 0 si la apasat butonul de reset
        end if;
     end if;
  end if;


end process;


    NSL : process(clk,flagEnd)
    begin
    
        if rising_edge(clk) then
            case state is
                when idle =>
                   
                    if startd = '1' then
                        state <=countdown;
                    else state <= idle;
                    end if;
                                                    
                when countdown =>
                               
                    -- Timer and TU (Time is up) flag
--                    if flagEnd = '1' then -- TU era un semnal , am renuntat la conditie sa nu ingreunam codul, dar las comentat in caz ca vreau sa revin
--                        TU <= '1'; 
--                    else
--                        TU <='0';
--                    end if;
                    
                    --verificarea de butoane
                    if btnLd = '1' then
                        codIntrodus<= codIntrodus*10+1;
                    end if;
                    if btnCd = '1' then
                        codIntrodus<= codIntrodus*10+2;
                    end if;
                    if btnRd = '1' then
                        codIntrodus<= codIntrodus*10+3;
                    end if;
                    
                    if  flagEnd = '1' or (codIntrodus>=111) then   --if  TU = '1' or (codIntrodus>=111) then
                        state<= verification;
                    end if;    

                    
                when verification =>
                    if codIntrodus = cod and flagEnd = '0' then
                        led(3)<='1';
                        led(2)<='0';
                     elsif  codIntrodus /=cod or flagEnd ='1' then
                        led(2)<='1';
                        led(3)<='0';
                    end if;
                    
                    if rst = '1' then   --if (rst and ALM) = '1' then
                        state    <= idle;
                        flagEnd <= '0';  -- TU <= '0';
                        led(2) <='0';
                        led(3) <='0';
                        codIntrodus <= 0;
                    end if;

                    
                when others => state<=idle;
            end case;
        end if;
    end process;
    
                   led(0)<='1' when state=idle else '0';
                   led(1)<='0' when state=idle else '1';
                   rst1<= '0' when state = countdown else '1';
                   led15(15)<=clk1hz;
                   
generate_score : process(rst1,clk1hz,secunde)
  variable thousand : integer range 0 to 9 := 0;
  variable hundred : integer range 0 to 9 := 0;
  variable ten : integer range 0 to 9 := 0;
  variable unit : integer range 0 to 9 := 0;
begin
  if rst1 = '1' then
    thousand := 0;
    hundred := 0;
    ten := 0;
    unit := 0;
  elsif rising_edge(clk1hz) then    --peste tot pe unde sunt secunde se poate schimba cu timer 
      if secunde <=9 then 
         unit :=secunde;
      elsif secunde <=99 then
         unit  := secunde mod 10;
         ten   := (secunde / 10) mod 10;
       elsif secunde <=999 then 
          hundred  := (secunde / 100) mod 10;
          ten      := (secunde / 10) mod 10;
          unit     := secunde mod 10;
       else 
          thousand := secunde / 1000;
          hundred  := (secunde / 100) mod 10;
          ten      := (secunde / 10) mod 10;
          unit     := secunde mod 10;
        end if;
 
    end if;  
  
  score <= std_logic_vector(to_unsigned(thousand,4)) &
           std_logic_vector(to_unsigned(hundred,4)) &
           std_logic_vector(to_unsigned(ten,4)) &
           std_logic_vector(to_unsigned(unit,4));
  
end process;
                                    
                   
u7seg : driver7seg port map (clk => clk,
                             Din => score,
                             an => an,
                             seg => seg,
                             dp_in => (others =>'0'),
                             dp_out => dp,
                             rst => rst);                   
                   
            
end Behavioral;
