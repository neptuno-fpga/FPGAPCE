//
// data_io.v
//
// io controller writable ram for the MiST board
// http://code.google.com/p/mist-board/
//
// Copyright (c) 2014 Till Harbaum <till@harbaum.org>
//
// This source file is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This source file is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

module data_io_pce # ( parameter STRLEN =   0 )
(
    // io controller spi interface
    input               sck,
    input               ss,
    input               sdi,
    output              sdo,
    
     input      [ 7:0] data_in,
    input      [(8*STRLEN)-1:0] conf_str,
    output reg [31:0] status,

    output reg          downloading,   // signal indicating an active download
    output reg [7:0]    index,         // menu index used to upload the file

    // external ram interface
    input               clk,
    input               clkref,
    output reg          wr,
    output reg [24:0]   a,
    output reg [7:0]    d
);

parameter START_ADDR = 25'h0;

// *********************************************************************************
// spi client
// *********************************************************************************
reg        sdo_s;
assign sdo = sdo_s;

reg [6:0]      sbuf;
reg [7:0]      cmd;
reg [7:0]      data;
reg [4:0]      cnt;

reg [24:0]     addr;
reg rclk;

localparam UIO_FILE_TX      = 8'h53;
localparam UIO_FILE_TX_DAT  = 8'h54;
localparam UIO_FILE_INDEX   = 8'h55;

reg       downloading_reg = 1'b0;
reg [7:0] index_reg;

reg [ 7:0] ACK = 8'd75; // letter K - 0x4b
reg [10:0] byte_cnt;   // counts bytes
    
// SPI MODE 0 : incoming data on Rising, outgoing on Falling
always@(negedge sck, posedge ss) 
begin
    
    //each time the SS goes down, we will receive a command from the SPI master
    if (ss) // not selected
    begin
        sdo_s    <= 1'bZ;
        byte_cnt <= 11'd0;
    end
    else
    begin

        if (cmd == 8'h10 ) //command 0x10 - send the data to the microcontroller
            sdo_s <= data_in[~cnt[2:0]];

        else if (cmd == 8'h00 ) //command 0x00 - ACK
            sdo_s <= ACK[~cnt[2:0]];

        //  else if (cmd == 8'h61 ) //command 0x61 - echo the pumped data
        //      sdo_s <= sram_data_s[~cnt[2:0]];            


        else if(cmd == 8'h14) //command 0x14 - reading config string
        begin

            if(byte_cnt < STRLEN + 1 ) // returning a byte from string
                sdo_s <= conf_str[{STRLEN - byte_cnt,~cnt[2:0]}];
            else
                sdo_s <= 1'b0;
        end 

        if(cnt[2:0] == 7) 
            byte_cnt <= byte_cnt + 8'd1;

    end
end

// data_io has its own SPI interface to the io controller
always@(posedge sck, posedge ss) begin

    reg [ 4:0] cnf_byte;

    if(ss == 1'b1)
    begin
        cnt <= 5'd0;
        cnf_byte <= 4'd15;
    end
    else begin
        rclk <= 1'b0;

        // don't shift in last bit. It is evaluated directly
        // when writing to ram
        if(cnt != 15)
            sbuf <= { sbuf[5:0], sdi};

        // increase target address after write
        if(rclk)
            addr <= addr + 25'd1;

        // count 0-7 8-15 8-15 ... 
        if(cnt < 15)    cnt <= cnt + 4'd1;
        else cnt <= 4'd8;

 // finished command byte
        if(cnt == 7) 
        begin 
            cmd <= {sbuf, sdi};
        
        
                // command 0x61: start the data streaming
                if(sbuf[6:0] == 7'b0110000 && sdi == 1'b1)
                begin
                    //addr <= 0; //nao pode estar aqui, senao zera o endereço a cada ciclo de dados tem que estar no 0x60
                    downloading_reg <= 1;
                end
                
                // command 0x62: end the data streaming
                if(sbuf[6:0] == 7'b0110001 && sdi == 1'b0)
                begin
                    //addr_w <= addr;
                    downloading_reg <= 0;
                end
        end
        
        if(cnt == 15) 
        begin 
        
                // command 0x15: stores the status word (menu selections)
                if (cmd == 8'h15)
                begin
                    case (cnf_byte) 
                                        
                        4'd15: status[31:24] <={sbuf, sdi};
                        4'd14: status[23:16] <={sbuf, sdi};
                        4'd13: status[15:8]  <={sbuf, sdi};
                        4'd12: status[7:0]   <={sbuf, sdi};
                        
//                        4'd11: core_mod <= {sbuf[5:0], sdi};
                    endcase
                    
                    cnf_byte <= cnf_byte - 1'd1;

                end
        
            // command 0x60: stores a configuration byte
                if (cmd == 8'h60)
                begin
//                        config_buffer_o[cnf_byte] <= {sbuf, sdi};
                        cnf_byte <= cnf_byte - 1'd1;
                        
                        addr <= 0;
                end
                        
                // command 0x61: Data Pump 8 bits
                if (cmd == 8'h61) 
                begin
                        a <= addr;
                        data <= {sbuf, sdi};
                        rclk <= 1;
                end
        end

         // expose file (menu) index
        if((cmd == UIO_FILE_INDEX) && (cnt == 15)) index_reg <= {sbuf, sdi};

/*

        // finished command byte
         if(cnt == 7)
            cmd <= {sbuf, sdi};

        // prepare/end transmission
        if((cmd == UIO_FILE_TX) && (cnt == 15)) begin
            // prepare
            if(sdi) begin
                addr <= START_ADDR;
                downloading_reg <= 1'b1; 
            end else
                downloading_reg <= 1'b0; 
        end

        // command 0x54: UIO_FILE_TX
        if((cmd == UIO_FILE_TX_DAT) && (cnt == 15)) begin
            data <= {sbuf, sdi};
            rclk <= 1'b1;
            a <= addr;
        end

        // expose file (menu) index
        if((cmd == UIO_FILE_INDEX) && (cnt == 15))
            index_reg <= {sbuf[3:0], sdi};
 */
    end
end

always@(posedge clk) begin
    // bring rclk from spi clock domain into core clock domain
    reg rclkD, rclkD2;
    reg wr_int;

    rclkD <= rclk;
    rclkD2 <= rclkD;
    wr <= 0;

    downloading <= downloading_reg;
    index <= index_reg;

    if (clkref) begin
        wr_int <= 0;
        if (wr_int) begin
            d <= data;
            wr <= 1'b1;
        end
    end

    if(rclkD && !rclkD2)
        wr_int <= 1'b1;
end

endmodule