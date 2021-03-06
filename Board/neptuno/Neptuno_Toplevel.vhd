--//============================================================================
--//
--//  neptUNO Top by Victor Trucco
--//
--//============================================================================

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;
use ieee.std_logic_unsigned.all;

entity neptUNO_Toplevel is
    port
    (
  
        -- Clocks
        clock_50_i         : in    std_logic;

        -- SRAM (IS61WV102416BLL-10TLI)
        sram_addr_o        : out   std_logic_vector(19 downto 0)   := (others => '0');
        sram_data_io       : inout std_logic_vector(15 downto 0)    := (others => 'Z');
        sram_we_n_o        : out   std_logic                               := '1';
        sram_oe_n_o        : out   std_logic                               := '1';
        sram_ub_n_o        : out   std_logic                               := '1';
        sram_lb_n_o        : out   std_logic                               := '1';

        -- SDRAM
        SDRAM_A            : out std_logic_vector(12 downto 0);
        SDRAM_DQ           : inout std_logic_vector(15 downto 0);

        SDRAM_BA           : out std_logic_vector(1 downto 0);
        SDRAM_DQMH         : out std_logic;
        SDRAM_DQML         : out std_logic;    

        SDRAM_nRAS         : out std_logic;
        SDRAM_nCAS         : out std_logic;
        SDRAM_CKE          : out std_logic;
        SDRAM_CLK          : out std_logic;
        SDRAM_nCS          : out std_logic;
        SDRAM_nWE          : out std_logic;
    
        -- PS2
        ps2_clk_io         : inout std_logic                        := 'Z';
        ps2_data_io        : inout std_logic                        := 'Z';
        ps2_mouse_clk_io   : inout std_logic                        := 'Z';
        ps2_mouse_data_io  : inout std_logic                        := 'Z';

        -- SD Card
        sd_cs_n_o          : out   std_logic                        := 'Z';
        sd_sclk_o          : out   std_logic                        := 'Z';
        sd_mosi_o          : out   std_logic                        := 'Z';
        sd_miso_i          : in    std_logic;

        -- Joysticks
        joy_clock_o        : out   std_logic;
        joy_load_o         : out   std_logic;
        joy_data_i         : in    std_logic;
        joy_p7_o           : out   std_logic                        := '1';

        -- Audio
        AUDIO_L             : out   std_logic                       := '0';
        AUDIO_R             : out   std_logic                       := '0';
        ear_i               : in    std_logic;
        mic_o               : out   std_logic                       := '0';
		  
        -- I2S audio
        SCLK				: out   std_logic								:= '0';
        LRCLK				: out   std_logic								:= '0';
        SDIN				: out   std_logic								:= '0';			  

        -- VGA
        VGA_R               : out   std_logic_vector(4 downto 0)    := (others => '0');
        VGA_G               : out   std_logic_vector(4 downto 0)    := (others => '0');
        VGA_B               : out   std_logic_vector(4 downto 0)    := (others => '0');
        VGA_HS              : out   std_logic                       := '1';
        VGA_VS              : out   std_logic                       := '1';

        LED                 : out   std_logic                       := '1';-- 0 is led on

        --STM32
        stm_rx_o            : out std_logic     := 'Z'; -- stm RX pin, so, is OUT on the slave
        stm_tx_i            : in  std_logic     := 'Z'; -- stm TX pin, so, is IN on the slave
        stm_rst_o           : out std_logic     := 'Z'; -- '0' to hold the microcontroller reset line, to free the SD card
        
        SPI_SCK             : in  std_logic;
        SPI_DO              : out std_logic   := 'Z';
        SPI_DI              : in  std_logic;
        SPI_SS2             : in  std_logic;
        SPI_nWAIT           : out std_logic   := '1' 
    );
END entity;

architecture rtl of neptUNO_Toplevel is
 type config_array is array(natural range 15 downto 0) of std_logic_vector(7 downto 0);
    signal config_buffer_s : config_array;

    component neptuno_joydecoder is
    port
    (
        clk_i           : in  std_logic;
        joy_data_i      : in  std_logic;
        joy_clk_o       : out  std_logic;
        joy_load_o      : out  std_logic;

        joy1_up_o       : out std_logic;
        joy1_down_o     : out std_logic;
        joy1_left_o     : out std_logic;
        joy1_right_o    : out std_logic;
        joy1_fire1_o    : out std_logic;
        joy1_fire2_o    : out std_logic;
        joy2_up_o       : out std_logic;
        joy2_down_o     : out std_logic;
        joy2_left_o     : out std_logic;
        joy2_right_o    : out std_logic;
        joy2_fire1_o    : out std_logic;
        joy2_fire2_o    : out std_logic
    );
    end component;


    component data_io
    generic
    (
       STRLEN : integer :=   0
    );
    port
    (
        clk_sys                   : in std_logic;
        SPI_SCK, SPI_SS2, SPI_DI :in std_logic;
        SPI_DO : out std_logic;

        data_in       : in std_logic_vector(7 downto 0);
        conf_str       : in std_logic_vector((8*STRLEN)-1 downto 0);
        status        : out std_logic_vector(31 downto 0);

        config_buffer_o : out config_array;



      --  clkref_n          : in  std_logic := '0';
        ioctl_download    : out std_logic;
        ioctl_index       : out std_logic_vector(7 downto 0);
        ioctl_wr          : out std_logic;
        ioctl_addr        : out std_logic_vector(24 downto 0);
        ioctl_dout        : out std_logic_vector(7 downto 0)
    );
  end component;

  component mist_video
generic (
    OSD_COLOR    : std_logic_vector(2 downto 0) := "110";
    OSD_X_OFFSET : std_logic_vector(9 downto 0) := (others => '0');
    OSD_Y_OFFSET : std_logic_vector(9 downto 0) := (others => '0');
    SD_HCNT_WIDTH: integer := 9;
    COLOR_DEPTH  : integer := 6;
    OSD_AUTO_CE  : boolean := true;
    USE_FRAMEBUFFER :integer := 0
);
port (
    clk_sys     : in std_logic;

    SPI_SCK     : in std_logic;
    SPI_SS3     : in std_logic;
    SPI_DI      : in std_logic;

    scanlines   : in std_logic_vector(1 downto 0);
    ce_divider  : in std_logic := '0';
    scandoubler_disable : in std_logic;

    rotate      : in std_logic_vector(1 downto 0);
  
    blend       : in std_logic := '0';
    no_csync    : in std_logic := '0';

    HSync       : in std_logic;
    VSync       : in std_logic;
    R           : in std_logic_vector(COLOR_DEPTH-1 downto 0);
    G           : in std_logic_vector(COLOR_DEPTH-1 downto 0);
    B           : in std_logic_vector(COLOR_DEPTH-1 downto 0);

    VGA_HS      : out std_logic;
    VGA_VS      : out std_logic;
    VGA_R       : out std_logic_vector(4 downto 0);
    VGA_G       : out std_logic_vector(4 downto 0);
    VGA_B       : out std_logic_vector(4 downto 0);
    osd_enable      : out std_logic
);
end component mist_video;

    component PumpSignal
    port(
        clk_i       : in  std_logic;
        reset_i     : in  std_logic;
        download_i  : in  std_logic;   
        pump_o      : out std_logic_vector( 7 downto 0)
    );
    end component;

signal reset        : std_logic;
signal reset_d      : std_logic;
signal pll_locked   : std_logic;
signal clk42m       : std_logic;
signal memclk       : std_logic;

signal audiol : std_logic_vector(23 downto 0);
signal audior : std_logic_vector(23 downto 0);
--
signal pce_red : std_logic_vector(2 downto 0);
signal pce_green : std_logic_vector(2 downto 0);
signal pce_blue : std_logic_vector(2 downto 0);
signal pce_vs : std_logic;
signal pce_hs : std_logic;

-- user_io
signal buttons: std_logic_vector(1 downto 0);
signal status:  std_logic_vector(31 downto 0);
signal joy_0: std_logic_vector(15 downto 0);
signal joy_1: std_logic_vector(15 downto 0);
signal joy_2: std_logic_vector(31 downto 0);
signal joy_3: std_logic_vector(31 downto 0);
signal joy_4: std_logic_vector(31 downto 0);
signal joyn_0: std_logic_vector(11 downto 0);
signal joyn_a: std_logic_vector(11 downto 0);
signal joyn_1: std_logic_vector(11 downto 0);
signal joyn_b: std_logic_vector(11 downto 0);
signal joyn_2: std_logic_vector(11 downto 0);
signal joyn_3: std_logic_vector(11 downto 0);
signal joyn_4: std_logic_vector(11 downto 0);
signal joy_ana_0: std_logic_vector(15 downto 0);
signal joy_ana_1: std_logic_vector(15 downto 0);

signal ypbpr : std_logic;
signal scandoubler_disable : std_logic;
signal no_csync : std_logic;

-- data_io
signal downloading      : std_logic;
signal data_io_wr       : std_logic;
signal data_io_clkref   : std_logic;
signal data_io_d        : std_logic_vector(7 downto 0);
signal data_io_index    : std_logic_vector(7 downto 0);
signal downloadingD     : std_logic;
signal d_state          : std_logic_vector(1 downto 0);

-- external controller signals
signal ext_reset_n      : std_logic_vector(2 downto 0) := "111";
signal ext_bootdone     : std_logic_vector(2 downto 0) := "000";
signal ext_data         : std_logic_vector(15 downto 0);
signal ext_data_req     : std_logic;
signal ext_data_ack     : std_logic := '0';
signal ext_sw           : std_logic_vector( 15 downto 0); --DIP switches

signal btn_n_o          : std_logic_vector(4 downto 1);


constant CONF_STR : string :=
    "P,PC Engine.ini;" &
    "S0,BIN/PCE,Load BIN PCE;"&
    "S1,SGX,Load SGX;"&
    "OBC,Scanlines,Off,25%,50%,75%;"&
    "O7,Blending,Off,On;"&
    "O8,Scandoubler,On,Off;" &	 
    "O6,Joystick swap,Off,On;"&
    "O3,6 Buttons,Disable,Enable;"&
    "O4,Multitap,Off,On;"&
    "O2,ROM data swap,Off,On;"&
    "T0,Reset;";

function to_slv(s: string) return std_logic_vector is
    constant ss: string(1 to s'length) := s;
    variable rval: std_logic_vector(1 to 8 * s'length);
    variable p: integer;
    variable c: integer;
  
  begin  
    for i in ss'range loop
      p := 8 * i;
      c := character'pos(ss(i));
      rval(p - 7 to p) := std_logic_vector(to_unsigned(c,8));
    end loop;
    return rval;

end function;

-- Sigma Delta audio
COMPONENT hybrid_pwm_sd
    PORT
    (
        clk     :    IN STD_LOGIC;
        n_reset     :    IN STD_LOGIC;
        din     :    IN STD_LOGIC_VECTOR(15 DOWNTO 0);
        dout        :    OUT STD_LOGIC
    );
END COMPONENT;

component data_io_pce
    generic
    (
       STRLEN : integer :=   0
    );
    port (  clk         : in std_logic;
            clkref      : in std_logic;
            wr          : out std_logic;
            a           : out std_logic_vector(24 downto 0);
            d           : out std_logic_vector(7 downto 0);
            downloading : out std_logic;
            index       : out std_logic_vector(7 downto 0);

        data_in       : in std_logic_vector(7 downto 0);
        conf_str       : in std_logic_vector((8*STRLEN)-1 downto 0);
        status        : out std_logic_vector(31 downto 0);


            sck         : in std_logic;
            ss          : in std_logic;
            sdi         : in std_logic;
            sdo         : out std_logic
        );
    end component;


  signal joy1_up_i, joy1_down_i, joy1_left_i, joy1_right_i, joy1_p6_i, joy1_p9_i : std_logic;
  signal joy2_up_i, joy2_down_i, joy2_left_i, joy2_right_i, joy2_p6_i, joy2_p9_i : std_logic;

    signal joy1_s       : std_logic_vector(15 downto 0) := (others => '1'); 
    signal joy2_s       : std_logic_vector(15 downto 0) := (others => '1'); 

    signal osd_s        : std_logic_vector(7 downto 0) := (others => '1'); 
    signal keys_s       : std_logic_vector(7 downto 0) := (others => '1'); 
    signal key_code     : std_logic_vector(7 downto 0) := (others => '0'); 
    signal key_strobe   : std_logic;

    signal clock_div_q  : std_logic_vector(1 downto 0);
    signal osd_enable   : std_logic;

    signal direct_video_s : std_logic;
    signal direct_video   : std_logic;	 

begin

-- -----------------------------------------------------------------------
-- Defaults
-- -----------------------------------------------------------------------

 neptuno_joydecoder1 : neptuno_joydecoder 
    port map
    (
        clk_i           => clock_50_i,
        joy_data_i      => joy_data_i,
        joy_clk_o       => joy_clock_o,
        joy_load_o      => joy_load_o,

        joy1_up_o       => joy1_up_i,
        joy1_down_o     => joy1_down_i,
        joy1_left_o     => joy1_left_i,
        joy1_right_o    => joy1_right_i,
        joy1_fire1_o    => joy1_p6_i,
        joy1_fire2_o    => joy1_p9_i,

        joy2_up_o       => joy2_up_i,
        joy2_down_o     => joy2_down_i,
        joy2_left_o     => joy2_left_i,
        joy2_right_o    => joy2_right_i,
        joy2_fire1_o    => joy2_p6_i,
        joy2_fire2_o    => joy2_p9_i
    );
  sram_we_n_o <= '1';
  sram_oe_n_o <= '1';
  stm_rst_o <= 'Z';

  PumpSignal_1 : PumpSignal port map(clk42m, (not pll_locked), downloading, osd_s);

  U00 : entity work.pll
    port map(
        inclk0 => clock_50_i,     -- external
        c0     => SDRAM_CLK,      -- 126Mhz external
        c1     => memclk,         -- 126Mhz internal
        c2     => clk42m,         -- 42.8 internal
        locked => pll_locked
    );

ext_sw(0) <= '1'; -- 15kHz output
ext_sw(2) <= status(2); -- rom data swap
ext_sw(3) <= status(3); -- 6 buttons
ext_sw(4) <= status(4); -- multitap
ext_sw(5) <= data_io_index(0); -- SGX mode

-- reset from IO controller
-- status bit 0 is always triggered by the i ocontroller on its own reset
-- button 1 is the core specfic button in the mists front
process(clk42m)
begin
    if rising_edge(clk42m) then
        reset_d <= NOT status(0) and pll_locked;
        reset <= reset_d;

        clock_div_q <= clock_div_q + 1;

    end if;
end process;

virtualtoplevel : entity work.Virtual_Toplevel
    generic map
    (
        colAddrBits => 9,
        rowAddrBits => 13
    )
    port map(
        reset => reset,
        CLK => clk42m,
        SDR_CLK => memclk,

        -- SDRAM ports
        DRAM_CKE => SDRAM_CKE,
        DRAM_CS_N => SDRAM_nCS,
        DRAM_RAS_N => SDRAM_nRAS,
        DRAM_CAS_N => SDRAM_nCAS,
        DRAM_WE_N => SDRAM_nWE,
        DRAM_UDQM => SDRAM_DQMH,
        DRAM_LDQM => SDRAM_DQML,
        DRAM_BA_1 => SDRAM_BA(1),
        DRAM_BA_0 => SDRAM_BA(0),
        DRAM_ADDR => SDRAM_A,
        DRAM_DQ => SDRAM_DQ,

        -- Joystick ports (Port_A, Port_B)
        joya => joyn_0,
        joyb => joyn_1,
        joyc => joyn_2,
        joyd => joyn_3,
        joye => joyn_4,

        -- Video, Audio/CMT ports
        R => pce_red,
        G => pce_green,
        B => pce_blue,

        HS => pce_hs,
        VS => pce_vs,

        DAC_LDATA => audiol,
        DAC_RDATA => audior,

        ext_reset_n  => ext_reset_n(2) and ext_reset_n(1) and ext_reset_n(0),
        ext_bootdone => ext_bootdone(2) or ext_bootdone(1) or ext_bootdone(0),
        ext_data     => ext_data,
        ext_data_req => ext_data_req,
        ext_data_ack => ext_data_ack,
        ext_sw => ext_sw
);

direct_video <= not status(8) xor direct_video_s;

mist_video1 : mist_video
    generic map (
        OSD_COLOR   => "001", -- blue
        COLOR_DEPTH => 3,
        SD_HCNT_WIDTH => 11,
        USE_FRAMEBUFFER => 0
    )
    port map (
        clk_sys     => clk42m,
        scanlines   => status(12 downto 11),
        scandoubler_disable => direct_video,
        no_csync    => no_csync,
        rotate      => "00",
        ce_divider  => '1',
        blend       => status(7),

        SPI_SCK     => SPI_SCK,
        SPI_SS3     => SPI_SS2,
        SPI_DI      => SPI_DI,

        HSync       => pce_hs,
        VSync       => pce_vs,
        R           => pce_red,
        G           => pce_green,
        B           => pce_blue,

        VGA_HS      => VGA_HS,
        VGA_VS      => VGA_VS,
        VGA_R       => VGA_R,
        VGA_G       => VGA_G,
        VGA_B       => VGA_B,

        osd_enable  => osd_enable
    );

LED <= not downloading;
--LED <= data_io_index(0);
--user_io_d : user_io
--    generic map (STRLEN => CONF_STR'length)
--    port map (
--        clk_sys => clk42m,
--        clk_sd  => clk42m,
--        SPI_CLK => SPI_SCK,
--        SPI_SS_IO => CONF_DATA0,
--        SPI_MISO => SPI_DO,
--        SPI_MOSI => SPI_DI,
--        conf_str => to_slv(CONF_STR),
--        status => status,
--        ypbpr => ypbpr,
--        no_csync => no_csync,
--        scandoubler_disable => scandoubler_disable,
--
--        joystick_0 => joy_0,
--        joystick_1 => joy_1,
--        joystick_2 => joy_2,
--        joystick_3 => joy_3,
--        joystick_4 => joy_4,
--        joystick_analog_0 => joy_ana_0,
--        joystick_analog_1 => joy_ana_1,
----      switches => switches,
--        BUTTONS => buttons
-- );

data_io_inst: data_io_pce
 generic map (STRLEN => CONF_STR'length)
    port map (
        clk     => memclk,
        clkref  => data_io_clkref,
        wr      => data_io_wr,
        a       => open,
        d       => data_io_d,
        downloading => downloading,
        index   => data_io_index,

        sck     => SPI_SCK,
        ss      => SPI_SS2,
        sdi     => SPI_DI,
        sdo     => SPI_DO,

        data_in => osd_s and keys_s,
        conf_str => to_slv(CONF_STR),
        status => status

    );

process(memclk)
begin
    if rising_edge( memclk ) then
        downloadingD <= downloading;
        ext_reset_n <= ext_reset_n(1 downto 0)&'1'; --stretch reset
        ext_bootdone <= ext_bootdone(1 downto 0)&'0';
        ext_data_ack <= '0';
        if (downloadingD = '0' and downloading = '1') then
            -- ROM downloading start
            ext_reset_n(0) <= '0';
            d_state <= "00";
            data_io_clkref <= '1';
        elsif (downloading = '0') then
            -- ROM downloading finished
            ext_bootdone(0) <= '1';
            data_io_clkref <= '0';
        elsif (downloading = '1') then
            -- ROM downloading in progress
            case d_state is
            when "00" =>
                if data_io_wr = '1' then
                    ext_data(7 downto 0) <= data_io_d;
                    data_io_clkref <= '1';
                    d_state <= "01";
                end if;
            when "01" =>
                if data_io_wr = '1' then
                    ext_data(15 downto 8) <= data_io_d;
                    data_io_clkref <= '0';
                    d_state <= "10";
                end if;
            when "10" =>
                if ext_data_req = '1' then
                    ext_data_ack <= '1';
                    d_state <= "11";
                end if;
            when "11" =>
                data_io_clkref <= '1';
                d_state <= "00";
            end case;
        end if;
    end if;
end process;


-- JOYSTICK 1 & 2 REMAP
---------------------------
--             109876543210
--(FPGA) joy_x MXYZSACBUDLR
--(PCE )joyn_x 6543RS21LDRU
-- -------------------------


-- swap, invert and remap joystick bits

 --joyn_a <= not joy_0(11 downto 4) & not joy_0(1) & not joy_0(2) & not joy_0(0) & not joy_0(3);
 --joyn_b <= not joy_1(11 downto 4) & not joy_1(1) & not joy_1(2) & not joy_1(0) & not joy_1(3);
 joyn_a <=  not joy_0(8) & not joy_0(9) & not joy_0(10) & not joy_0(6) & not joy_0(7) & not joy_0(11) & not joy_0(4) & not joy_0(5) & not joy_0(1) & not joy_0(2) & not joy_0(0) & not joy_0(3);
 joyn_b <=  not joy_1(8) & not joy_1(9) & not joy_1(10) & not joy_1(6) & not joy_1(7) & not joy_1(11) & not joy_1(4) & not joy_1(5) & not joy_1(1) & not joy_1(2) & not joy_1(0) & not joy_1(3); 
 joyn_0 <= joyn_a when status(6) = '0' else joyn_b;
 joyn_1 <= joyn_b when status(6) = '0' else joyn_a;
 joyn_2 <= not joy_2(11 downto 4) & not joy_2(1) & not joy_2(2) & not joy_2(0) & not joy_2(3);
 joyn_3 <= not joy_3(11 downto 4) & not joy_3(1) & not joy_3(2) & not joy_3(0) & not joy_3(3);
 joyn_4 <= not joy_4(11 downto 4) & not joy_4(1) & not joy_4(2) & not joy_4(0) & not joy_4(3);

-- Do we have audio?  If so, instantiate a two DAC channels.
leftsd: component hybrid_pwm_sd
    port map
    (
        clk => memclk,
        n_reset => reset,
        din => not audiol(23) & std_logic_vector(audiol(22 downto 8)),
        dout => AUDIO_L
    );

rightsd: component hybrid_pwm_sd
    port map
    (
        clk => memclk,
        n_reset => reset,
        din => not audior(23) & std_logic_vector(audior(22 downto 8)),
        dout => AUDIO_R
    );

-- I2S audio
audio_i2s: entity work.audio_top
	port map(
		clk_50MHz => clock_50_i,
		dac_MCLK  => open,
		dac_LRCK  => LRCLK,
		dac_SCLK  => SCLK,
		dac_SDIN  => SDIN,
		L_data    => std_logic_vector(audiol(23 downto 8)), --not audiol(23) & std_logic_vector(audiol(22 downto 8)), 
		R_data    => std_logic_vector(audiol(23 downto 8)) --not audior(23) & std_logic_vector(audior(22 downto 8))
	);		 
	 
	 
  --------------------------------------------------------------------------------------

    -- translate scancode to joystick
    joystick : entity work.MC2_HID
    generic map 
    (
        osd_cmd     => "011",
        USE_VKP     => '1',
        CLK_SPEED   => 12000
    )
    port map 
    (
        clk         => clock_div_q(1),
        kbd_clk     => ps2_clk_io,
        kbd_dat     => ps2_data_io,
        osd_o       => keys_s,
        osd_enable  => osd_enable,
        direct_video => direct_video_s,
           
        -- 1,2,u,d,l,r   
        joystick_0  => joy1_p9_i & joy1_p6_i & joy1_up_i & joy1_down_i & joy1_left_i & joy1_right_i,
        joystick_1  => joy2_p9_i & joy2_p6_i & joy2_up_i & joy2_down_i & joy2_left_i & joy2_right_i,

        -- joystick_0 and joystick_1 should be swapped
        joyswap     => '0',

        -- player1 and player2 should get both joystick_0 and joystick_1
        oneplayer   => '0',

        -- tilt, coin4-1, start4-1
        controls => open,
        
        -- fire12-1, up, down, left, right

        player1    => joy_0,
        player2    => joy_1,

        -- sega joystick
        sega_strobe => joy_p7_o ,

        front_buttons_i => "1111",
        front_buttons_o => open

    );

end architecture;
