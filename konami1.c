// license:BSD-3-Clause
// copyright-holders:Olivier Galibert
/***************************************************************************
  The Konami_1 CPU is a 6809 with opcodes scrambled.
***************************************************************************/

/*
#include "emu.h"
#include "konami1.h"

const device_type KONAMI1 = &device_creator<konami1_device>;

konami1_device::konami1_device(const machine_config &mconfig, const char *tag, device_t *owner, UINT32 clock)
	: m6809_base_device(mconfig, "KONAMI-1", tag, owner, clock, KONAMI1, 1, "konami1", __FILE__)
{
	m_boundary = 0x0000;
}

void konami1_device::device_start()
{
	m_mintf = new mi_konami1(m_boundary);
	m6809_base_device::device_start();
}

void konami1_device::set_encryption_boundary(UINT16 adr)
{
	m_boundary = adr;
	if(m_mintf)
		static_cast<mi_konami1 *>(m_mintf)->m_boundary = adr;
}

konami1_device::mi_konami1::mi_konami1(UINT16 adr)
{
	m_boundary = adr;
}

UINT8 konami1_device::mi_konami1::read_opcode(UINT16 adr)
{
	UINT8 val = m_sdirect->read_byte(adr);
	if(adr < m_boundary)
		return val;
	switch(adr & 0xa) {
	default:
	case 0x0: return val ^ 0x22;
	case 0x2: return val ^ 0x82;
	case 0x8: return val ^ 0x28;
	case 0xa: return val ^ 0x88;
	}
}
*/

#include "def.h"

UINT8 konami1_decodebyte( UINT8 opcode, UINT16 address )
{
/*
>
> CPU_D7 = (EPROM_D7 & ~ADDRESS_1) | (~EPROM_D7 & ADDRESS_1)  >
> CPU_D6 = EPROM_D6
>
> CPU_D5 = (EPROM_D5 & ADDRESS_1) | (~EPROM_D5 & ~ADDRESS_1) >
> CPU_D4 = EPROM_D4
>
> CPU_D3 = (EPROM_D3 & ~ADDRESS_3) | (~EPROM_D3 & ADDRESS_3) >
> CPU_D2 = EPROM_D2
>
> CPU_D1 = (EPROM_D1 & ADDRESS_3) | (~EPROM_D1 & ~ADDRESS_3) >
> CPU_D0 = EPROM_D0
>

*/
	UINT8 xormask;
	
	
	xormask = 0;
	if (address & 0x02) 
		xormask |= 0x80;
	else 
		xormask |= 0x20;
	if (address & 0x08) 
		xormask |= 0x08; 
	else 
		xormask |= 0x02;
	
	//printf("opcode %02x\n",opcode);
	return opcode ^ xormask;
	} 
