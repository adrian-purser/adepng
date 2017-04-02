//=============================================================================
//	FILE:			adepng.h
//	SYSTEM:			AdePNG
//	DESCRIPTION:	PNG Image Loading & Saving
//					This is a lightweight set of functions for working with
//					PNG images. This code is not fully compliant and cannot 
//					work with all PNG files. It is intended as an easy to use
//					lightweight way of loading and saving basic PNG files.
//-----------------------------------------------------------------------------
//  COPYRIGHT:		(C) Copyright 2013-2017 Adrian Purser. All Rights Reserved.
//	LICENCE:		MIT - See LICENSE file for details
//	MAINTAINER:		Adrian Purser <ade@adrianpurser.co.uk>
//	CREATED:		27-JUL-2013 Adrian Purser <ade@adrianpurser.co.uk>
//=============================================================================
//
//	Implemented Features
//
//	- = No Planned Support
//	* = Implemented
//	? = Implemented but untested.
//
//	+-----------------------------------------+---------------+---------------+
//	| FEATURE                                 |    DECODER    |    ENCODER    |
//	+-----------------------------------------+---------------+---------------+
//  | COLOUR FORMATS                          |OUT COMPONENTS |               |
//  |                                         |---+---+---+---|               |
//  |                                         | 1 | 2 | 3 | 4 |               |
//  |                                         |---+---+---+---|               |
//  |     True Colour               8-bit     | * | * | * | * |               |
//  |                              16-bit     | - | - | - | - |      ---      |
//  |     True Colour with Alpha    8-bit     | * | * | * | * |               |
//  |                              16-bit     | - | - | - | - |      ---      |
//  |     GreyScale                 8-bit     | * | * | * | * |               |
//  |                              16-bit     | - | - | - | - |      ---      |
//  |                             < 8-bit     | * | * | * | * |               |
//  |     Greyscale with Alpha      8-bit     | ? | ? | ? | ? |               |
//  |                              16-bit     | - | - | - | - |      ---      |
//  |                             < 8-bit     | ? | ? | ? | ? |               |
//  |     Index Colour              8-bit     | * | * | * | * |               |
//  |                             < 8-bit     | * | * | * | * |               |
//  |                                         |   |   |   |   |               |
//	+-----------------------------------------+---+---+---+---+---------------+
//  | FILTERING                               |     DEPTH     |     DEPTH     |
//  |                                         +-----+----+----+-----+----+----+
//  |     Type 0                              | <8  | 8  | 16 | <8  | 8  | 16 |
//  |                                         +-----+----+----+-----+----+----+
//  |         Mode 0 - No filtering           |  *  | *  | *  |     |    |    |
//  |         Mode 1 - Sub                    |  *  | *  | -  |     |    |    |
//  |         Mode 2 - Up                     |  *  | *  | -  |     |    |    |
//  |         Mode 3 - Average                |  *  | *  | -  |     |    |    |
//  |         Mode 4 - Paeth                  |  *  | *  | -  |     |    |    |
//	+-----------------------------------------+---------------+---------------+
//  | MISC                                    |               |
//  |                                         |               |
//  |     Interlaced Images                   |      ---      |
//  |     Non-Interlaced Images               |      YES      |
//  |                                         |               |
//	+-----------------------------------------+---------------+
//  | CHUNKS                                  |               |
//  |                                         |               |
//  |     IHDR                                |      YES      |
//  |     IDAT                                |      YES      |
//  |     PLTE                                |      YES      |
//  |     tEXt                                |      YES      |
//  |     iTXt (Non-Compressed)               |      YES      |
//  |          (Compressed)                   |               |
//  |                                         |               |
//	+-----------------------------------------+---------------+
//
//*****************************************************************************

#ifndef GUARD_ADEPNG_H
#define GUARD_ADEPNG_H

#include <string>
#include <vector>
#include <cstdint>
#include <memory>
#include <functional>

#define HAVE_MINIZ

namespace adepng
{

#define PNG_CHUNK_OVERHEAD 12
#define PNG_CHUNK_HEADER_SIZE 8
#define PNG_CHUNK_FOOTER_SIZE 4

enum ColourType
{
	GREYSCALE			= 0,
	TRUECOLOUR			= 2,
	INDEXED_COLOUR		= 3,
	GREYSCALE_ALPHA		= 4,
	TRUECOLOUR_ALPHA	= 6
};


typedef std::function<void(const unsigned char *,size_t)> png_write_func;

struct TextField
{
	std::string					keyword;
	std::string					text;
	std::string					language;
//	bool						b_international;
//	bool						b_compressed;
};

class PNGEncode
{
private:
	static const size_t				m_idat_chunk_size = 0x08000;
	std::uint8_t					m_compression;
	std::uint8_t					m_filter;
	std::uint8_t					m_interlace;

	std::vector<TextField>			m_text;		


public:
	PNGEncode(void);
	~PNGEncode(void);

	int				encode(	int						width,
							int						height,
							int						depth,
							ColourType				type,
							const unsigned char *	p_data,
							png_write_func			write_func,
							int						compression_level = 0);

	void			add_text(	const std::string & keyword,
								const std::string & text);

	void			add_text(	const std::string & keyword,
								const std::string & text,
								const std::string & language );

private:
	inline void		write_long(std::uint32_t v,unsigned char * p) {p[0]=(v>>24)&0x0FF;p[1]=(v>>16)&0x0FF;p[2]=(v>>8)&0x0FF;p[3]=v&0x0FF;}
	void			write_text(png_write_func write_func);

	void			write_chunk(					std::uint32_t			chunk_id,
													const unsigned char *	p_data,
													size_t					datasize,
													png_write_func			write_func);

	void			write_IHDR_chunk(				int						width,
													int						height,
													int						depth,
													ColourType				type,
													png_write_func			write_func );

	void			write_tEXt_chunk(				TextField *				p_text,
													png_write_func			write_func );

	void			write_iTXt_chunk(				TextField *				p_text,
													png_write_func			write_func );

	void			write_image_data(				int						width,
													int						height,
													int						depth,
													ColourType				type,
													const unsigned char *	p_data,
													png_write_func			write_func,
													int						compression_level );

	void			write_image_data_uncompressed(	int						width,
													int						height,
													int						depth,
													ColourType				type,
													const unsigned char *	p_data,
													png_write_func			write_func );

	int				write_image_data_compressed(	int						width,
													int						height,
													int						depth,
													ColourType				type,
													const unsigned char *	p_data,
													png_write_func			write_func,
													int						compression_level );

};


class PNGDecode
{
private:
	enum
	{
		LOG_WARNING,
		LOG_ERROR,
		LOG_INFO
	};

	std::vector<TextField>			m_text;
	int								m_width				= 0;
	int								m_height			= 0;
	int								m_colourtype		= 0;
	int								m_depth				= 0;
	int								m_compression		= 0;
	int								m_filter			= 0;
	int								m_interlace			= 0;

	int								m_samples			= 0;		// The number of samples per pixel (eg. RGBA = 4)
	int								m_out_components	= 0;		// The number of components (samples) to be written to the output.

	size_t							m_output_offset		= 0;
	std::vector<std::uint8_t>		m_image_data;

	std::vector<std::uint32_t>		m_palette;

	std::vector<std::uint8_t>		m_scanlines[2];					// Buffers for building scanlines.
	int								m_active_scanline	= 0;		// The current scanline being built;
	int								m_scanline_size		= 0;		// The size of a scanline including filter byte.
	int								m_scanline_offset	= 0;
	const std::uint8_t *			m_p_last_scanline	= nullptr;

	std::string						m_error_string;

public:
	int						decode(const char * p_data,size_t datasize,int components=0,bool b_smallmem=true);
	int						decode2(const char * p_data,size_t datasize,int components=0,bool b_smallmem=true);

	int						width() const		{return m_width;}
	int						height() const		{return m_height;}
	int						colourtype() const	{return m_colourtype;}
	int						components() const	{return m_out_components;}
	const std::uint8_t *	get_data() const	{return &m_image_data[0];}
	size_t					datasize() const	{return m_image_data.size();}

private:
	std::uint32_t			read32(const unsigned char * p_data)	{return (p_data[0] << 24) | (p_data[1] << 16) | (p_data[2] << 8) | p_data[3];}
	std::uint16_t			read16(const unsigned char * p_data)	{return (p_data[0] << 8) | p_data[1];}

	int						log_error(const std::string & message,int severity=LOG_ERROR);

	int						process_chunk_ihdr(const std::uint8_t * p_data,size_t datasize);
	int						process_chunk_text(const std::uint8_t * p_data,size_t datasize);
	int						process_chunk_itxt(const std::uint8_t * p_data,size_t datasize);
	int						process_chunk_plte(const std::uint8_t * p_data,size_t datasize);

	int						decode_image_data(const std::uint8_t * p_data,size_t datasize);
	void					process_image_data_segment(const std::uint8_t * p_data,size_t datasize);

	void					filter_scanline(std::uint8_t * p_data,const std::uint8_t * p_last_data);
	void					process_scanline(std::uint8_t * p_data,const std::uint8_t * p_last_data);
	void					process_scanline_truecolour_alpha(const std::uint8_t * p_src_data,std::uint8_t * p_out_data);
	void					process_scanline_truecolour(const std::uint8_t * p_src_data,std::uint8_t * p_out_data);
	void					process_scanline_greyscale(const std::uint8_t * p_src_data,std::uint8_t * p_out_data);
	void					process_scanline_greyscale_alpha(const std::uint8_t * p_src_data,std::uint8_t * p_out_data);
	void					process_scanline_indexed(const std::uint8_t * p_src_data,std::uint8_t * p_out_data);

	std::uint32_t			palette(size_t index) const {return (index < m_palette.size() ? m_palette[index] : 0x0FF000000);}
	std::uint32_t			palette(size_t index,std::uint8_t & out_r,std::uint8_t & out_g,std::uint8_t & out_b) const 
							{
								const auto clr = (index < m_palette.size() ? m_palette[index] : 0);
								out_r = (clr>>16)&0x0FF;
								out_g = (clr>>8)&0x0FF;
								out_b = clr&0x0FF;
								return clr;
							}
	std::uint8_t			palette_grey(size_t index) const 
							{
								const auto clr = (index < m_palette.size() ? m_palette[index] : 0x0FF000000);
								return static_cast<std::uint8_t>((((clr>>16)&0x0FF) + ((clr>>8)&0x0FF) + (clr&0x0FF)) / 3);
							}

};


} // namespace adepng

#endif // ! defined GUARD_ADEPNG_H
