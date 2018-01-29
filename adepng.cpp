//=============================================================================
//	FILE:			adepng.cpp
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

#include <iostream>
#include <fstream>
#include <algorithm>
#include <cassert>
#include <array>
#include <cstring>
#include "adepng.h"

#ifdef HAVE_MINIZ
#define MINIZ_HEADER_FILE_ONLY
#define MINIZ_NO_STDIO
#define MINIZ_NO_ARCHIVE_APIS
#define MINIZ_NO_TIME
#define MINIZ_NO_ZLIB_APIS
#define MINIZ_NO_MALLOC
#include <miniz.c>
#endif

#define CHUNK_NAME(a,b,c,d) ( (((a)&0x0FF)<<24) | (((b)&0x0FF)<<16) | (((c)&0x0FF)<<8) | ((d)&0x0FF) )

#define CHUNK_IHDR CHUNK_NAME('I','H','D','R')
#define CHUNK_IDAT CHUNK_NAME('I','D','A','T')
#define CHUNK_PLTE CHUNK_NAME('P','L','T','E')
#define CHUNK_sBIT CHUNK_NAME('s','B','I','T')
#define CHUNK_sRGB CHUNK_NAME('s','R','G','B')
#define CHUNK_pHYs CHUNK_NAME('p','H','Y','s')
#define CHUNK_tEXt CHUNK_NAME('t','E','X','t')
#define CHUNK_iTXt CHUNK_NAME('i','T','X','t')
#define CHUNK_IEND CHUNK_NAME('I','E','N','D')

enum
{
	IHDR_WIDTH			= 0,
	IHDR_HEIGHT			= 4,
	IHDR_DEPTH			= 8,
	IHDR_COLOUR_TYPE,
	IHDR_COMPRESSION,
	IHDR_FILTER,
	IHDR_INTERLACE,
	IHDR_SIZE
};

namespace adepng
{

static unsigned char png_ident[8] = { 0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A };

//=============================================================================
//
//
//	INFLATE 
//
//
//=============================================================================

class InflateToCallback
{
public:
	enum Status
	{
		COMPLETE,
		INCOMPLETE,
		ERROR
	};

private:
	tinfl_decompressor									m_decompressor;
	std::vector<std::uint8_t>							m_output_buffer;
	std::function<int(const std::uint8_t *,size_t)>		m_callback;
	unsigned int										m_flags				= 0;
	size_t												m_total_out			= 0;
	size_t												m_out_ofs			= 0;

public:
	InflateToCallback() = default;
	~InflateToCallback() = default;
	InflateToCallback(const InflateToCallback &) = delete;
	InflateToCallback & operator=(const InflateToCallback &) = delete;

	void create(std::function<int(const std::uint8_t *,size_t)> callback,
				size_t											buffer_size = TINFL_LZ_DICT_SIZE )
	{
		m_callback = callback;
		tinfl_init(&m_decompressor);
		m_output_buffer.resize(buffer_size);
		m_flags		=	TINFL_FLAG_PARSE_ZLIB_HEADER | TINFL_FLAG_HAS_MORE_INPUT;
		m_total_out = 0;
		m_out_ofs	= 0;
	}

	int write(const std::uint8_t * p_data,size_t size)
	{
		int result = INCOMPLETE;
		size_t		in_buf_ofs = 0;
		mz_uint8 *	p_out = (mz_uint8 *)&m_output_buffer[0];

		while(	(result == INCOMPLETE)
				&& ((size-in_buf_ofs) > 0) )
		{
			//-----------------------------------------------------------------
			//	Decompress the next section of the input data.
			//-----------------------------------------------------------------
			size_t	in_buf_size		= size-in_buf_ofs;
			size_t	dst_buf_size	= m_output_buffer.size() - m_out_ofs;

			tinfl_status status		= tinfl_decompress(	
											&m_decompressor, 
											(const mz_uint8*)p_data + in_buf_ofs, 
											&in_buf_size, 
											p_out, 
											p_out + m_out_ofs, 
											&dst_buf_size,
											m_flags );

			in_buf_ofs	+= in_buf_size;
			m_out_ofs	+= dst_buf_size;
			m_total_out	+= dst_buf_size;

			//-----------------------------------------------------------------
			//	If finished or the buffer is full then flush the buffer 
			//	contents by calling the registered callback function.
			//-----------------------------------------------------------------
			if(	m_out_ofs &&
			   ((status <= TINFL_STATUS_DONE) || (m_out_ofs == m_output_buffer.size())) )
			{
				if(m_callback(&m_output_buffer[0],m_out_ofs))
				{
					result = ERROR;
					break;
				}

				m_out_ofs = 0;
			}

			//-----------------------------------------------------------------
			//	If we have finished then set the result accordingly which will
			//	cause the loop to finish.
			//-----------------------------------------------------------------
			if(status <= TINFL_STATUS_DONE)
				result = (status == TINFL_STATUS_DONE ? COMPLETE : ERROR);
		}

		return result;
	}
};

//=============================================================================
//
//
//	BITSTREAM 
//
//
//=============================================================================

class BitStreamIn
{
private:
	const std::uint8_t *	m_p_data			= nullptr;
	size_t					m_size				= 0;
	size_t					m_bits				= 0;
	size_t					m_buffer_size		= 0;
	std::uint16_t			m_buffer			= 0;

public:
	BitStreamIn() = default;
	~BitStreamIn() = default;
	BitStreamIn(const std::uint8_t *	p_data,
				size_t					size,
				size_t					bits )
				: m_p_data(p_data)
				, m_size(size)
				, m_bits(bits)
	{
		assert((bits > 0) && (bits <= 8));
	}

	size_t	size_bits() const	{return (m_size * 8) + m_buffer_size;}
	size_t	remainder() const	{return (m_bits ? (size_bits()%m_bits) : 0);}
	size_t	size() const		{return (m_bits ? (size_bits()/m_bits) : 0);}
	bool	empty() const		{return !!(size() > 0);}

	std::uint8_t read()
	{
		if(m_buffer_size < m_bits)
		{
			if(!m_size)
				return 0;
			m_buffer |= ((std::uint16_t)(*m_p_data++)) << (8-m_buffer_size);
			m_buffer_size += 8;
			--m_size;
		}

		const auto value = static_cast<std::uint8_t>((m_buffer >> (16-m_bits)) & 0x0FF);
		m_buffer_size -= m_bits;
		m_buffer <<= m_bits;

		return value;
	}

	std::uint8_t read_byte()	{return read() << (8-m_bits);}
	std::uint8_t read_byte_repl()
	{
		auto value = read() << (8-m_bits);
		std::uint8_t result = 0;
		for(size_t i=0;i<8;i+=m_bits)
			result |= (value>>i);
		return result;
	}

};


//=============================================================================
//
//
//	CRC 
//
//
//=============================================================================

static bool				sg_b_crc_table_valid = false;
static std::uint32_t	sg_crc_table[256];

static void make_crc_table()
{
	for(std::uint32_t n=0;n<256;++n) 
	{
		std::uint32_t c(n);
		for(std::uint32_t k=0;k<8;++k) 
		{
			if(c & 1)
				c = 0xedb88320L ^ (c >> 1);
			else
				c >>= 1;
		}
		sg_crc_table[n] = c;
	}
	sg_b_crc_table_valid = true;
}

static std::uint32_t update_crc(std::uint32_t crc, const unsigned char *buf,size_t len)
{
	if(!sg_b_crc_table_valid)
		make_crc_table();

	for(size_t n=0;n<len;++n) 
		crc = sg_crc_table[(crc ^ buf[n]) & 0xff] ^ (crc >> 8);

	return crc;
}

std::uint32_t calculate_crc(const unsigned char *buf, int len)
{
	return update_crc(0xffffffffL, buf, len) ^ 0xffffffffL;
}


//=============================================================================
//
//
//	PNG ENCODER 
//
//
//=============================================================================

PNGEncode::PNGEncode(void)
	: m_compression(0)
	, m_filter(0)
	, m_interlace(0)
{
}

PNGEncode::~PNGEncode(void)
{
}


int
PNGEncode::encode(	int						width,
					int						height,
					int						depth,
					ColourType				type,
					const unsigned char *	p_data,
					png_write_func			write_func,
					int						compression_level )
{
	if(!write_func)
		return -1;

	if(!p_data)
		return -1;

	//	Write the file header.
	write_func(png_ident,sizeof(png_ident));

	write_IHDR_chunk(width,height,depth,type,write_func);
	write_text(write_func);
	write_image_data(width,height,depth,type,p_data,write_func,compression_level);
	write_chunk(CHUNK_IEND,nullptr,0,write_func);

	return 0;
}

void							
PNGEncode::add_text(const std::string & keyword,const std::string & text)
{
	TextField field;
	field.text				= text;
	field.keyword			= keyword;
//	field.b_international	= false;
//	field.b_compressed		= false;
	m_text.push_back(field);
}


void
PNGEncode::add_text(const std::string & keyword,
					const std::string & text,
					const std::string & language )
{
	TextField field;
	field.text				= text;
	field.keyword			= keyword;
	field.language			= language;
//	field.b_international	= true;
//	field.b_compressed		= b_compress;
	m_text.push_back(field);
}

void
PNGEncode::write_chunk(	std::uint32_t			chunk_id,
						const unsigned char *	p_data,
						size_t					datasize,
						png_write_func			write_func )
{
	unsigned char tempdata[8];

	//	Write the header
	write_long(static_cast<std::uint32_t>(datasize),tempdata);
	write_long(chunk_id,&tempdata[4]);
	write_func(tempdata,8);
	std::uint32_t crc = update_crc(0xffffffffL, &tempdata[4], 4);

	//	Write the payload
	if(p_data && datasize)
	{
		write_func(p_data,datasize);
		crc = update_crc(crc,static_cast<const unsigned char *>(p_data),datasize);
	}

	//	Write the crc
	write_long(crc ^ 0xffffffffL,tempdata);
	write_func(tempdata,4);
}


void
PNGEncode::write_IHDR_chunk(int						width,
							int						height,
							int						depth,
							ColourType				type,
							png_write_func			write_func )
{
	std::uint8_t ihdr[IHDR_SIZE];

	write_long(width,&ihdr[IHDR_WIDTH]);
	write_long(height,&ihdr[IHDR_HEIGHT]);

	ihdr[IHDR_DEPTH]				= static_cast<std::uint8_t>(depth);
	ihdr[IHDR_COLOUR_TYPE]			= static_cast<std::uint8_t>(type);
	ihdr[IHDR_COMPRESSION]			= m_compression;
	ihdr[IHDR_FILTER]				= m_filter;
	ihdr[IHDR_INTERLACE]			= m_interlace;

	write_chunk(CHUNK_IHDR,ihdr,IHDR_SIZE,write_func);
}

void
PNGEncode::write_text(png_write_func write_func)
{
	for(auto & text : m_text)
	{
		if(!text.language.empty())
			write_iTXt_chunk(&text,write_func);
		else
			write_tEXt_chunk(&text,write_func);
	}
}

void
PNGEncode::write_tEXt_chunk(TextField * p_text,png_write_func write_func )
{
	if(p_text)
	{
		std::string data(p_text->keyword);
		data.push_back(0);
		data.append(p_text->text);
		
		write_chunk(CHUNK_tEXt,(const unsigned char *)data.c_str(),data.size(),write_func);
	}
}


void
PNGEncode::write_iTXt_chunk(TextField * p_text,png_write_func write_func )
{
	if(p_text)
	{
		std::string data(p_text->keyword);				// Keyword
		data.push_back(0);								// Keyword terminator
		data.push_back(0); //p_text->b_compressed ? 1:0);		// Compression flag. 0 = uncompressed. 1 = compressed.
		data.push_back(0);								// Compression method.
		data.append(p_text->language);					// Language tag.
		data.push_back(0);								// Language tag terminator.
		data.push_back(0);								// Translater keyword terminator.

		data.append(p_text->text);
		
		write_chunk(CHUNK_iTXt,(const unsigned char *)data.c_str(),data.size(),write_func);
	}
}

void
PNGEncode::write_image_data(int						width,
							int						height,
							int						depth,
							ColourType				type,
							const unsigned char *	p_data,
							png_write_func			write_func,
							int						compression_level )
{
	if(compression_level == 0)
		write_image_data_uncompressed(width,height,depth,type,p_data,write_func);
	else
		write_image_data_compressed(width,height,depth,type,p_data,write_func,compression_level);
}

void
PNGEncode::write_image_data_uncompressed(	int						width,
											int						height,
											int						depth,
											ColourType				type,
											const unsigned char *	p_data,
											png_write_func			write_func )
{
	std::array<unsigned char,m_idat_chunk_size>	buffer;
	
	static const char		bpp_table[8] = {1,0,3,1,2,0,4,0};
	size_t					bpp = (bpp_table[type & 7] * depth)/8;
	size_t					span = width * bpp;
	size_t					chunk_size = 0;
	bool					b_init = true;
	size_t					hdr_offset = 0;
	size_t					chunk_header_size = 0;

	for(int y=0;y<height;++y)
	{
		size_t remaining = (width*bpp)+1;
		bool b_filter = true;
		const unsigned char * p_rowdata = p_data + y*span;

		while(remaining || (chunk_size && (y==height-1)))
		{
			//-----------------------------------------------------------------
			//	If the chunk is empty then write the header to it.
			//-----------------------------------------------------------------
			if(chunk_size == 0)
			{
				// If this is the first chunk then write the ZIP header.
				if(b_init)
				{
					buffer[chunk_size++] = 0x78;	//	CMF (Compression Method and Flags)
					buffer[chunk_size++] = 0x01;	//	FLG (FLaGs, 0-4=FCHECK, 5=FDICT, 6-7=FLEVEL)
					b_init = false;
				}

				// Write the deflate header.
				hdr_offset = chunk_size;
				buffer[chunk_size] = 0x00;	//	BTYPE = 00 (No Compression).
				chunk_size += 5;
				chunk_header_size = chunk_size;
			}

			//-----------------------------------------------------------------
			//	Calculate the amount of data that we can write.
			//-----------------------------------------------------------------
			size_t available = (m_idat_chunk_size-chunk_size)-2;
			size_t actionable = std::min(remaining,available);

			//-----------------------------------------------------------------
			//	If the chunk is full or if there is no more data and the chunk
			//	is not empty then write the chunk.
			//-----------------------------------------------------------------
			if(!actionable && (chunk_size > chunk_header_size))
			{
				buffer[hdr_offset+2]	= ((chunk_size-chunk_header_size)>>8)&0x0FF;
				buffer[hdr_offset+1]	= (chunk_size-chunk_header_size)&0x0FF;
				buffer[hdr_offset+3]	= buffer[hdr_offset+1] ^ 0x0FF;
				buffer[hdr_offset+4]	= buffer[hdr_offset+2] ^ 0x0FF;

				// If this is the last block then set the BFINAL bit in the header.
				if(!remaining && (y==height-1))
				{
					buffer[hdr_offset] |= 1;
				//TODO: write adler32;
				//	chunk_size += 2;
				}

				write_chunk(CHUNK_IDAT,buffer.data(),chunk_size,write_func);
				chunk_size = 0;
			}
			else
			{
				//-------------------------------------------------------------
				//	If there is source data available then write it to the 
				//	chunk buffer.
				//-------------------------------------------------------------
				while(actionable)
				{
					// If this is the start of a row then write the filter byte.
					if(b_filter)
					{
						buffer[chunk_size++] = 0;
						b_filter = false;
						--actionable;
						--remaining;
					}
					else
					{
						std::memcpy(&buffer[chunk_size],p_rowdata,actionable);
						chunk_size+=actionable;
						p_rowdata+=actionable;
						remaining -= actionable;
						actionable = 0;
					}
				}
			}
		}
	}
}

int
PNGEncode::write_image_data_compressed(	int						width,
										int						height,
										int						depth,
										ColourType				type,
										const unsigned char *	p_data,
										png_write_func			write_func,
										int						/*compression_level*/ )
{
#ifdef HAVE_MINIZ

	static const char					bpp_table[8]		= {1,0,3,1,2,0,4,0};
	size_t								bpp					= (bpp_table[type & 7] * depth)/8;
	size_t								span				= width * bpp;

//	const size_t						buffer_size = ((width+1) * bpp * height)+64;
	std::unique_ptr<tdefl_compressor>	p_compressor = std::make_unique<tdefl_compressor>();
//	tdefl_compressor 					compressor;
	tdefl_compressor *					p_comp = p_compressor.get();

	struct WriterInfo
	{
		PNGEncode *			p_encoder;
		png_write_func		write_func;
	};

	WriterInfo writer;
	writer.p_encoder = this;
	writer.write_func = write_func;

	tdefl_init(	p_comp,
				[](const void *pBuf, int len, void *pUser)->mz_bool
				{
					const unsigned char * p_data = (const unsigned char *)pBuf;
					WriterInfo * p_writer = (WriterInfo *)pUser;
					while(len)
					{
						int size = std::min(len,(int)m_idat_chunk_size);
						p_writer->p_encoder->write_chunk(CHUNK_IDAT,p_data,size,p_writer->write_func);
						len -= size;
						p_data += size;
					}
					return MZ_TRUE;
				},
				&writer,
				TDEFL_DEFAULT_MAX_PROBES | TDEFL_WRITE_ZLIB_HEADER );

	const char filter = 0;
	for(int y=0;y<height;++y)
	{
		tdefl_compress_buffer(p_comp,&filter,1,TDEFL_NO_FLUSH);
		tdefl_compress_buffer(p_comp,p_data + y*span,span,TDEFL_NO_FLUSH);
	}

	if(tdefl_compress_buffer(p_comp,nullptr,0,TDEFL_FINISH) != TDEFL_STATUS_DONE)
		return -1;

#else
	write_image_data_uncompressed(width,height,depth,type,p_data,write_func);
#endif
	return 0;
}


//=============================================================================
//
//
//	PNG DECODER
//
//
//=============================================================================

int				
PNGDecode::log_error(const std::string & message,int severity)
{
	int result = 0;

	m_error_string.reserve(m_error_string.size() + 16);

	switch(severity)
	{
		case LOG_WARNING :	m_error_string.append("WARNING: "); break;
		case LOG_ERROR :	m_error_string.append("ERROR: "); result = -1;break;
		default :			m_error_string.append("INFO: "); break;
	}

	m_error_string.append(message);
	m_error_string.push_back(10);

	switch(severity)
	{
		case LOG_WARNING :	std::clog << "WARNING: " << message << std::endl; break;
		case LOG_ERROR :	std::clog << "ERROR: " << message << std::endl; break;
		default :			std::clog << "INFO: " << message << std::endl; break;
	}

	return result;
}
/*
int
PNGDecode::decode(const char * p_data,size_t datasize,int components,bool b_smallmem)
{

	m_width = 0;
	m_height = 0;

	if((components < 0) || (components > 4))
		m_out_components = 0;
	else
		m_out_components = components;

	//-------------------------------------------------------------------------
	//	Validate the header.
	//-------------------------------------------------------------------------
	if(datasize<8)
		return log_error("The file is too short for a valid PNG");

	static unsigned char ident[8] = { 0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A };
	if(memcmp(p_data,ident,8))
		return log_error("The file is not valid PNG format!");

	p_data += 8;
	datasize -= 8;

	//-------------------------------------------------------------------------
	//	Scan the chunks.
	//-------------------------------------------------------------------------
	struct PNGChunk
	{
		std::uint32_t			type;
		size_t					size;
		const std::uint8_t *	p_data;
	};

	std::vector<PNGChunk> chunks;

	PNGChunk				ihdr_chunk {0,0,nullptr};
	std::vector<PNGChunk>	idat_chunks;



	const unsigned char * p_dat = (const unsigned char *)p_data;
	bool b_finished = false;

	while(datasize && !b_finished)
	{
		//---------------------------------------------------------------------
		//	Check that there is enough data for the chunk header.
		//---------------------------------------------------------------------
		if(datasize < PNG_CHUNK_OVERHEAD)
		{
			log_error("Ignoring extra data at end of image file!",LOG_WARNING);
			b_finished = true;
			continue;
		}
	
		//---------------------------------------------------------------------
		//	Read the chunk length,type & CRC.
		//---------------------------------------------------------------------
		std::uint32_t	length	= read32(p_dat);
		std::uint32_t	type	= read32(p_dat+4);
		PNGChunk		chunk	{type,length,p_dat+PNG_CHUNK_HEADER_SIZE};

		if(length > datasize)
		{
			log_error("Missing data for chunk! Ignoring Chunk!",LOG_WARNING);
			b_finished = true;
			continue;
		}

		std::uint32_t chunk_crc	= read32(p_dat+PNG_CHUNK_HEADER_SIZE+length);
		std::uint32_t calc_crc	= calculate_crc((const unsigned char *)(p_dat+4),length+4);

		p_dat += PNG_CHUNK_OVERHEAD+length;
		datasize -= PNG_CHUNK_OVERHEAD+length;

		if(chunk_crc != calc_crc)
		{
			log_error("CRC Check Failed! Ignoring Chunk!",LOG_WARNING);
			continue;
		}

		//---------------------------------------------------------------------
		//	Update the chunk list.
		//---------------------------------------------------------------------

		chunks.push_back(chunk);

		switch(type)
		{
			case CHUNK_IHDR :	ihdr_chunk = chunk; break;
			case CHUNK_IDAT :	idat_chunks.push_back(chunk); break;
			case CHUNK_IEND :	b_finished = true; break;
			default :			break;
		}
	}

	//-------------------------------------------------------------------------
	//	Check whether we have the mandatory chunks.
	//-------------------------------------------------------------------------
	if(!ihdr_chunk.p_data)
		return log_error("IHDR chunk is missing!");

	if(idat_chunks.empty())
		return log_error("Missing image data!");


	//-------------------------------------------------------------------------
	//	Scan the chunks.
	//-------------------------------------------------------------------------
	bool b_error = false;

	for(const auto & chunk : chunks)
	{
		switch(chunk.type)
		{
			case CHUNK_IHDR : b_error = !!process_chunk_ihdr(chunk.p_data,chunk.size); break;	
			case CHUNK_PLTE : b_error = !!process_chunk_plte(chunk.p_data,chunk.size); break;	
			case CHUNK_tEXt : process_chunk_text(chunk.p_data,chunk.size); break;	
			case CHUNK_iTXt : process_chunk_itxt(chunk.p_data,chunk.size); break;	

			default : break;
		}

		if(b_error)
			break;
	}

	if(b_error)
		return -1;

	//-------------------------------------------------------------------------
	//	Process the image data.
	//-------------------------------------------------------------------------
	if(b_smallmem)
	{
		InflateToCallback inflate;

		inflate.create(	[&](const std::uint8_t * p_data,size_t size)->int
						{
							//std::cout << "INFLATE: " << size << std::endl;
							process_image_data_segment(p_data,size);
							return 0;
						},
						32 * 1024 );

		for(const auto & chunk : idat_chunks)
		{
			inflate.write(chunk.p_data,chunk.size);
		}
	}
	else
	{
		InflateToCallback inflate;
		size_t image_size = m_scanline_size * (m_height+1);

		if(image_size)
		{
			std::vector<std::uint8_t> imagedata;
			imagedata.resize(image_size);
			size_t offset = 0;

			inflate.create(	[&](const std::uint8_t * p_data,size_t size)->int
							{
								//std::cout << "INFLATE: " << size << std::endl;
								std::memcpy(&imagedata[offset],p_data,size);
								offset += size;
								return 0;
							},
							32 * 1024 );

			for(const auto & chunk : idat_chunks)
			{
				inflate.write(chunk.p_data,chunk.size);
			}

			std::uint8_t * p_imgdata = &imagedata[0];
			std::uint8_t * p_last = nullptr;
			std::uint8_t * p_out = &m_image_data[0];

			for(int h=m_height;h;--h,p_imgdata+=m_scanline_size)
			{
				filter_scanline(p_imgdata,p_last);
				p_last = p_imgdata;
			}

			p_imgdata = &imagedata[1];
			const size_t out_line_size = m_width * m_out_components;

			switch(m_colourtype)
			{
				case GREYSCALE :			for(int h=m_height;h;--h,p_out += out_line_size,p_imgdata += m_scanline_size)process_scanline_greyscale(p_imgdata,p_out); break;
				case TRUECOLOUR :			for(int h=m_height;h;--h,p_out += out_line_size,p_imgdata += m_scanline_size)process_scanline_truecolour(p_imgdata,p_out); break;
				case INDEXED_COLOUR :		for(int h=m_height;h;--h,p_out += out_line_size,p_imgdata += m_scanline_size)process_scanline_indexed(p_imgdata,p_out); break;
				case GREYSCALE_ALPHA :		for(int h=m_height;h;--h,p_out += out_line_size,p_imgdata += m_scanline_size)process_scanline_greyscale_alpha(p_imgdata,p_out); break;
				case TRUECOLOUR_ALPHA :		for(int h=m_height;h;--h,p_out += out_line_size,p_imgdata += m_scanline_size)process_scanline_truecolour_alpha(p_imgdata,p_out); break;
				default :					break;
			}

		}
	}
*/

	/*

	const unsigned char * p_dat = (const unsigned char *)p_data;
	bool b_finished = false;
	bool b_error = false;

	std::vector<std::uint8_t>	imgdata;
	
	while((datasize >= PNG_CHUNK_OVERHEAD) && !b_finished && !b_error)
	{
		//---------------------------------------------------------------------
		//	Read the chunk length and type.
		//---------------------------------------------------------------------
		std::uint32_t length = read32(p_dat);
		std::uint32_t type	 = read32(p_dat+4);

		if((length+PNG_CHUNK_OVERHEAD) > datasize)
		{
			log_error("Missing data for chunk!",WARNING);
			break;
		}

		std::uint32_t file_crc = read32(p_dat+8+length);
		std::uint32_t calc_crc = calculate_crc((const unsigned char *)&p_dat[4],length+4);
		
		p_dat += 8;
		datasize -= 8;


		//---------------------------------------------------------------------
		//	Process the chunk.
		//---------------------------------------------------------------------
		switch(type)
		{
			//-----------------------------------------------------------------
			case CHUNK_IHDR :
			//-----------------------------------------------------------------
				b_error = process_chunk_ihdr(p_dat,datasize);
				break;

			//-----------------------------------------------------------------
			case CHUNK_sBIT :
			//-----------------------------------------------------------------
				break;

			//-----------------------------------------------------------------
			case CHUNK_sRGB : 
			//-----------------------------------------------------------------
				break;

			//-----------------------------------------------------------------
			case CHUNK_pHYs :
			//-----------------------------------------------------------------
				break;

			//-----------------------------------------------------------------
			case CHUNK_tEXt :
			//-----------------------------------------------------------------
				{
					size_t key_size = strlen((const char *)p_dat);
					TextField textfield;
					textfield.keyword			= std::string((const char *)&p_dat[0],key_size);
					textfield.text				= std::string((const char *)&p_dat[key_size+1],(length-key_size)-1);
					textfield.b_compressed		= false;
					textfield.b_international	= false;
					m_text.push_back(textfield);
				}
				break;

			//-----------------------------------------------------------------
			case CHUNK_iTXt :
			//-----------------------------------------------------------------
				{
					size_t size		= strlen((const char *)p_dat);
					size_t offset	= 0;

					TextField textfield;

					textfield.keyword			= std::string((const char *)&p_dat[offset],size);
					offset += size+1;

					offset += 2;
					size = strlen((const char *)&p_dat[offset]);
					textfield.language = (size ? std::string((const char *)&p_dat[offset],size) : std::string("unknown"));

					offset += size+1;
					size = strlen((const char *)&p_dat[offset]);

					offset+= size+1;
					textfield.text				= std::string((const char *)&p_dat[offset],length-offset);

					textfield.b_compressed		= false;
					textfield.b_international	= false;
					m_text.push_back(textfield);
				}
				break;

			//-----------------------------------------------------------------
			case CHUNK_IDAT :
			//-----------------------------------------------------------------
				if(length>0)
				{
					auto offset = imgdata.size();
					imgdata.resize(offset+length);
					std::memcpy(&imgdata[offset],p_dat,length);
				}
				break;

			//-----------------------------------------------------------------
			case CHUNK_IEND :
			//-----------------------------------------------------------------
				b_finished = true;
				break;

			//-----------------------------------------------------------------
			default :
			//-----------------------------------------------------------------
				break;
		}

		p_dat += length+4;
		datasize -= (length+4);

	}

	//-------------------------------------------------------------------------
	//	Validate the image.
	//-------------------------------------------------------------------------
	int pixelcount = m_width * m_height;

	if(pixelcount <= 0)
		return log_error("Image dimensions invalid!");

	if(imgdata.empty())
		return log_error("Missing Image Data!");
		
	if(decode_image_data(&imgdata[0],imgdata.size()))
		return log_error("Failed to decode image data!");

	*/
/*
	return 0;
}
*/


int
PNGDecode::decode2(const char * p_data,size_t datasize,int components,bool /*b_smallmem*/)
{

	m_width = 0;
	m_height = 0;

	if((components < 0) || (components > 4))
		m_out_components = 0;
	else
		m_out_components = components;

	//-------------------------------------------------------------------------
	//	Validate the header.
	//-------------------------------------------------------------------------
	if(datasize<8)
		return log_error("The file is too short for a valid PNG");

	static unsigned char ident[8] = { 0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A };
	if(memcmp(p_data,ident,8))
		return log_error("The file is not valid PNG format!");

	p_data += 8;
	datasize -= 8;


	//-------------------------------------------------------------------------
	//	Create the Inflator to inflate chunk data.
	//-------------------------------------------------------------------------
	InflateToCallback inflate;

	inflate.create(	[&](const std::uint8_t * p_data,size_t size)->int
					{
						//std::cout << "INFLATE: " << size << std::endl;
						process_image_data_segment(p_data,size);
						return 0;
					},
					32 * 1024 );

	//-------------------------------------------------------------------------
	//	Scan the chunks.
	//-------------------------------------------------------------------------
	bool b_ihdr_chunk = false;

	const unsigned char * p_dat = (const unsigned char *)p_data;
	bool b_finished = false;
	bool b_error = false;

	while(datasize && !b_finished && !b_error)
	{
		//---------------------------------------------------------------------
		//	Check that there is enough data for the chunk header.
		//---------------------------------------------------------------------
		if(datasize < PNG_CHUNK_OVERHEAD)
		{
			log_error("Ignoring extra data at end of image file!",LOG_WARNING);
			b_finished = true;
			continue;
		}
	
		//---------------------------------------------------------------------
		//	Read the chunk length,type & CRC.
		//---------------------------------------------------------------------
		std::uint32_t	length			= read32(p_dat);
		std::uint32_t	type			= read32(p_dat+4);
		std::uint8_t *	p_chunk_data	= (std::uint8_t *)p_dat+PNG_CHUNK_HEADER_SIZE;

		if(length > datasize)
		{
			log_error("Missing data for chunk! Ignoring Chunk!",LOG_WARNING);
			b_finished = true;
			continue;
		}

		std::uint32_t chunk_crc	= read32(p_dat+PNG_CHUNK_HEADER_SIZE+length);
		std::uint32_t calc_crc	= calculate_crc((const unsigned char *)(p_dat+4),length+4);

		p_dat += PNG_CHUNK_OVERHEAD+length;
		datasize -= PNG_CHUNK_OVERHEAD+length;

		if(chunk_crc != calc_crc)
		{
			log_error("CRC Check Failed! Ignoring Chunk!",LOG_WARNING);
			continue;
		}

		//---------------------------------------------------------------------
		//	Update the chunk list.
		//---------------------------------------------------------------------

		switch(type)
		{
			case CHUNK_IHDR : b_error = !!process_chunk_ihdr(p_chunk_data,length); b_ihdr_chunk = !b_error; break;	
			case CHUNK_PLTE : b_error = !!process_chunk_plte(p_chunk_data,length); break;	
			case CHUNK_tEXt : process_chunk_text(p_chunk_data,length); break;	
			case CHUNK_iTXt : process_chunk_itxt(p_chunk_data,length); break;	
			case CHUNK_IEND : b_finished = true; break;
			case CHUNK_IDAT : if(b_ihdr_chunk) inflate.write(p_chunk_data,length); else b_error = true; break;

			default : break;
		}

	}

	if(b_error)
		return -1;

	return 0;
}

int				
PNGDecode::process_chunk_text(const std::uint8_t * p_data,size_t datasize)
{
	//-------------------------------------------------------------------------
	//	Extract the keyword and text from the data.
	//-------------------------------------------------------------------------
	const std::uint8_t * p_text {nullptr};
	const std::uint8_t * p_src {p_data};
	const std::uint8_t * p_end {p_data+datasize};

	for(size_t i=1;(i<(datasize-1)) && !p_text;++i)
		if(*p_src++ == 0)
			p_text = p_src;

	if(!p_text)
		return log_error("Invalid 'tEXt' chunk!",LOG_WARNING);

	size_t text_size = 0;
	const std::uint8_t * p_text_end {p_text};

	while(*p_text_end++ && (p_text_end<p_end))
		++text_size;

	//-------------------------------------------------------------------------
	//	Create a testfield object.
	//-------------------------------------------------------------------------
	TextField textfield;
	textfield.keyword			= std::string((const char *)&p_data[0]);
	textfield.text				= std::string((const char *)p_text,text_size);
//	textfield.b_compressed		= false;
//	textfield.b_international	= false;
	m_text.push_back(textfield);

	return 0;
}


int				
PNGDecode::process_chunk_itxt(const std::uint8_t * p_data,size_t datasize)
{
	if(datasize<6)
		return log_error("Invalid itxt chunk! (too small)",LOG_WARNING);

	size_t offset = 0;
	size_t size = 0;

	TextField textfield;

	//-------------------------------------------------------------------------
	// Keyword - 1-79 bytes (character string)
	//           1 byte (null terminator)
	//-------------------------------------------------------------------------
	const std::uint8_t * p_text = p_data;
	while((offset<datasize) && p_data[offset])
	{
		++size;
		++offset;
	}

	++offset;

	if((size<1) || ((datasize-offset)<4))
		return log_error("Invalid itxt chunk! (keyword)",LOG_WARNING);

	textfield.keyword.assign((const char *)p_text,size);

	//-------------------------------------------------------------------------
	// Compression Flag   - 1 byte
	// Compression Method - 1 byte
	//-------------------------------------------------------------------------
	auto comp_flag		= p_data[offset++];
	//auto comp_method	= p_data[offset++];

	if(comp_flag)
		return log_error("Compressed itxt chunk currently not supported!",LOG_WARNING);

	//-------------------------------------------------------------------------
	// Language tag - 0 or more bytes (character string)
	//                1 byte (null terminator)
	//-------------------------------------------------------------------------
	size = 0;
	p_text = p_data + offset;
	while((offset<datasize) && p_data[offset])
	{
		++size;
		++offset;
	}

	++offset;

	if((datasize-offset)<1)
		return log_error("Invalid itxt chunk! (language tag)",LOG_WARNING);

	if(size > 0)
		textfield.language.assign((const char *)p_text,size);

	//-------------------------------------------------------------------------
	// Translated keyword - 0 or more bytes
	//                      1 byte (null terminator)
	//-------------------------------------------------------------------------
	size = 0;
	p_text = p_data + offset;
	while((offset<datasize) && p_data[offset])
	{
		++size;
		++offset;
	}

	if(offset >= datasize)
		return log_error("Invalid itxt chunk! (translated keyword)",LOG_WARNING);

	++offset;

	//-------------------------------------------------------------------------
	// Text - 0 or more bytes
	//-------------------------------------------------------------------------
	size = datasize - offset;

	if(size > 0)
		textfield.text.assign((const char *)(p_data+offset),size);
		
	//-------------------------------------------------------------------------
	//	Add the text field.
	//-------------------------------------------------------------------------
	m_text.push_back(textfield);

	return 0;


	//size_t size		= strlen((const char *)p_dat);
	//size_t offset	= 0;

	//TextField textfield;

	//textfield.keyword			= std::string((const char *)&p_dat[offset],size);
	//offset += size+1;

	//offset += 2;
	//size = strlen((const char *)&p_dat[offset]);
	//textfield.language = (size ? std::string((const char *)&p_dat[offset],size) : std::string("unknown"));

	//offset += size+1;
	//size = strlen((const char *)&p_dat[offset]);

	//offset+= size+1;
	//textfield.text				= std::string((const char *)&p_dat[offset],length-offset);

	//textfield.b_compressed		= false;
	//textfield.b_international	= false;
	//m_text.push_back(textfield);




}


int
PNGDecode::process_chunk_ihdr(const std::uint8_t * p_data,size_t /*datasize*/)
{
	m_width			= read32(p_data);
	m_height		= read32(&p_data[4]);
	m_depth			= p_data[8];
	m_colourtype	= p_data[9];
	m_compression	= p_data[10];
	m_filter		= p_data[11];
	m_interlace		= p_data[12];

	//-------------------------------------------------------------------------
	// Ensure that the depth value is a supported value.
	//-------------------------------------------------------------------------
	switch(m_depth)
	{
		case 1 :
		case 2 :
		case 4 :
		case 8 :	
		case 16 :	break;
		default :	
			return log_error("Invalid Bit Depth!");
	}

	//-------------------------------------------------------------------------
	// Calculate the sample count.
	//-------------------------------------------------------------------------
	switch(m_colourtype)
	{
		case GREYSCALE :			m_samples = 1;	break;
		case TRUECOLOUR :			m_samples = 3;	break;
		case INDEXED_COLOUR :		m_samples = 1;	
									if(m_depth == 16) 
										return log_error("Invalid Depth for Colour Type!");
									break;

		case GREYSCALE_ALPHA :		m_samples = 2;	break;
		case TRUECOLOUR_ALPHA :		m_samples = 4;	break;
		default :					
			return log_error("Invalid Colour Type!");
	}

	//-------------------------------------------------------------------------
	// Ensure that the depth is supported for the number of samples.
	//-------------------------------------------------------------------------
	if((m_samples > 1) && (m_depth != 8))
		return log_error("Invalid Depth for Colour Type!");

	//-------------------------------------------------------------------------
	// Calculate the size of the scanline and allocate some scanline buffers.
	//-------------------------------------------------------------------------
	m_scanline_size		= (((m_samples * m_depth * m_width) + 7) / 8) + 1;
	m_scanline_offset	= 0;
	m_scanlines[0].resize(m_scanline_size);
	m_scanlines[1].resize(m_scanline_size);

	//-------------------------------------------------------------------------
	// Allocate the image data.
	//-------------------------------------------------------------------------
	if(!m_out_components)
	{
		if(m_colourtype == INDEXED_COLOUR)
			m_out_components = 3;
		else
			m_out_components = m_samples;
	}

	m_image_data.resize(m_width * m_height * m_out_components);

/*
	std::cout	<< "IHDR: " << m_width << "x" << m_height << "x" << m_samples
				<< " depth=" << m_depth
				<< " outcomp=" << m_out_components
				<< std::endl;
*/ 
	return 0;
}

int						
PNGDecode::process_chunk_plte(const std::uint8_t * p_data,size_t datasize)
{
	if(m_colourtype != INDEXED_COLOUR)
		return 0;

	if(!m_depth)
		return log_error("Invalid depth while processing palette chunk!",LOG_ERROR);

	size_t src_count = datasize / 3;
	size_t max_count = size_t(1) << m_depth;
	size_t count = std::min(src_count,max_count);

	m_palette.clear();
	m_palette.resize(max_count,0x0FF000000);

	for(size_t i=0;i<count;++i,p_data+=3)
		m_palette[i] = (p_data[0]<<16) | (p_data[1]<<8) | p_data[2] | 0x0FF000000;

	return 0;
}

int
PNGDecode::decode_image_data(const std::uint8_t * p_data,size_t datasize)
{
	if(m_compression != 0)
		return -1;

	size_t len = datasize;

	auto status = tinfl_decompress_mem_to_callback(	
		p_data, 
		&len,
		[](const void* pBuf, int len, void *pUser)->int
		{
			// TODO: Do something with the de-compressed data.
			((PNGDecode *)pUser)->process_image_data_segment((const std::uint8_t *)pBuf,(size_t)len);
			return len;
		},
		this,
		TINFL_FLAG_PARSE_ZLIB_HEADER );

	if(!status)
	{
		std::cout << "tinfl_decompress_mem_to_callback() failed!" << std::endl;
		return -1;
	}

	return 0;
}


void			
PNGDecode::process_image_data_segment(const std::uint8_t * p_data,size_t datasize)
{
//	std::cout << "PROCESS: " << datasize << std::endl;

	while(datasize)
	{
		if(!m_scanline_offset && ((int)datasize >= m_scanline_size))
		{
			auto p_out = &m_scanlines[m_active_scanline][0];
			std::memcpy(p_out,p_data,m_scanline_size);
			process_scanline(p_out,m_p_last_scanline);
			p_data += m_scanline_size;
			datasize -= m_scanline_size;
			m_p_last_scanline = p_out;
			m_active_scanline = 1-m_active_scanline;
		}
		else
		{
			auto remain = m_scanline_size - m_scanline_offset;
			auto size = std::min<size_t>(remain,datasize);

			assert(size);

			std::memcpy(&m_scanlines[m_active_scanline][m_scanline_offset],p_data,size);
			p_data += size;
			datasize -= size;
			m_scanline_offset += (int)size;

			if(m_scanline_offset == m_scanline_size)
			{
				auto p_out = &m_scanlines[m_active_scanline][0];
				process_scanline(p_out,m_p_last_scanline);
				m_p_last_scanline = p_out;
				m_active_scanline = 1-m_active_scanline;
				m_scanline_offset = 0;
			}
		}
	}
}


static std::uint8_t 
paeth(std::uint8_t a,std::uint8_t b,std::uint8_t c)
{
	auto p = a + b - c;
	auto pa = (p < a ? a-p : p-a);
	auto pb = (p < b ? b-p : p-b);
	auto pc = (p < c ? c-p : p-c);

	if((pa <= pb) && (pa <= pc))
		return a;
	
	if(pb <= pc)
		return b;
	
	return c;
}

void 
PNGDecode::filter_scanline(std::uint8_t * p_data,const std::uint8_t * p_last_data)
{
	//	Filters may use the original values of the following bytes to generate 
	//	the new byte value:
	//
	//	x -	the byte being filtered;
	//	a -	the byte corresponding to x in the pixel immediately before the 
	//		pixel containing x (or the byte immediately before x, when the bit 
	//		depth is less than 8);
	//	b -	the byte corresponding to x in the previous scanline;
	//	c -	the byte corresponding to b in the pixel immediately before the 
	//		pixel containing b (or the byte immediately before b, when the bit 
	//		depth is less than 8).

	switch(p_data[0])
	{
		//---------------------------------------------------------------------
		case 0 :	// No Filter
		//---------------------------------------------------------------------
			break;

		//---------------------------------------------------------------------
		case 1 :	// Sub - Recon(x) = Filt(x) + Recon(a)	
		//---------------------------------------------------------------------
			p_data[0] = 0;
			if(m_depth < 8)
			{
				for(int i=2;i<m_scanline_size;++i)
					p_data[i] += p_data[i-1];
			}
			else if(m_depth == 8)
			{
				for(int i=m_samples+1;i<m_scanline_size;++i)
					p_data[i] += p_data[i-m_samples];
			}
			break;

		//---------------------------------------------------------------------
		case 2 : // Sub - Recon(x) = Filt(x) + Recon(b)	
		//---------------------------------------------------------------------
			//p_data[0] = 0;
			//if(p_last_data)
			//{
			//	for(int i=1;i<m_scanline_size;++i)
			//		p_data[i] += p_last_data[i];
			//}

			*p_data++ = 0;
			if(p_last_data++)
			{
				for(int i=m_scanline_size-1;i;--i,++p_data)
					*p_data += *p_last_data++;
			}

			break;

		//---------------------------------------------------------------------
		case 3 : // Average - Recon(x) = Filt(x) + floor((Recon(a)+Recon(b))/2)
		//---------------------------------------------------------------------
			p_data[0] = 0;
			if(p_last_data)
			{
				if(m_depth < 8)
				{
					for(int i=1;i<m_scanline_size;++i)
						p_data[i] += (( p_last_data[i] + p_data[i-1]) >> 1 );
				}
				else if(m_depth == 8)
				{
					for(int i=1;i<=m_samples;++i)
						p_data[i] += ( p_last_data[i] >> 1 );
					for(int i=m_samples+1;i<m_scanline_size;++i)
						p_data[i] += (( p_last_data[i] + p_data[i-m_samples]) >> 1 );
				}
			}
			else
			{
				if(m_depth < 8)
				{
					for(int i=1;i<m_scanline_size;++i)
						p_data[i] += (p_data[i-1] >> 1);
				}
				else if(m_depth == 8)
				{
					for(int i=m_samples+1;i<m_scanline_size;++i)
						p_data[i] += (p_data[i-m_samples] >> 1 );
				}
			}

			break;

		//---------------------------------------------------------------------
		case 4 : // Average - Filt(x) + Paeth(Recon(a), Recon(b), Recon(c))
		//---------------------------------------------------------------------
			p_data[0] = 0;
			if(p_last_data)
			{
				if(m_depth < 8)
				{
					for(int i=1;i<m_scanline_size;++i)
						p_data[i] += paeth(p_data[i-1],p_last_data[i],p_last_data[i-1]);
				}
				else if(m_depth == 8)
				{
					for(int i=1;i<=m_samples;++i)
						p_data[i] += paeth(0,p_last_data[i],0);

					for(int i=m_samples+1;i<m_scanline_size;++i)
						p_data[i] += paeth(p_data[i-m_samples],p_last_data[i],p_last_data[i-m_samples]);
				}
			}
			else
			{
				if(m_depth < 8)
				{
					for(int i=1;i<m_scanline_size;++i)
						p_data[i] += paeth(p_data[i-1],0,0);
				}
				else if(m_depth == 8)
				{
					//for(int i=1;i<=m_samples;++i)
					//	p_data[i] += paeth(0,0,0);

					for(int i=m_samples+1;i<m_scanline_size;++i)
						p_data[i] += paeth(p_data[i-m_samples],0,0);
				}
			}


			break;

		default :	break;
	}
}

void
PNGDecode::process_scanline(std::uint8_t * p_data,const std::uint8_t * p_last_data)
{
	//std::cout	<< "SCANLINE: filter = " 
	//			<< std::hex << (unsigned int)(p_data[0])
	//			<< std::dec << std::endl;

	//-------------------------------------------------------------------------
	//	Apply Scanline Filter.
	//-------------------------------------------------------------------------
	filter_scanline(p_data,p_last_data);

	//-------------------------------------------------------------------------
	//	
	//-------------------------------------------------------------------------
	++p_data;

	auto p_out = &m_image_data[m_output_offset];
	m_output_offset += (m_width * m_out_components);

	switch(m_colourtype)
	{
		case GREYSCALE :			process_scanline_greyscale(p_data,p_out); break;
		case TRUECOLOUR :			process_scanline_truecolour(p_data,p_out); break;
		case INDEXED_COLOUR :		process_scanline_indexed(p_data,p_out); break;
		case GREYSCALE_ALPHA :		process_scanline_greyscale_alpha(p_data,p_out); break;
		case TRUECOLOUR_ALPHA :		process_scanline_truecolour_alpha(p_data,p_out); break;
		default :					break;
	}
}

void
PNGDecode::process_scanline_truecolour_alpha(const std::uint8_t * p_src_data,std::uint8_t * p_out_data)
{
	if(m_depth != 8)
		return;

	switch(m_out_components)
	{
		//---------------------------------------------------------------------
		case 4 :
		//---------------------------------------------------------------------
//			memcpy(p_out_data,p_src_data,m_width*4);

			for(int w=m_width;w;--w,p_src_data+=4)
			{
				*p_out_data++ = p_src_data[2];
				*p_out_data++ = p_src_data[1];
				*p_out_data++ = p_src_data[0];
				*p_out_data++ = p_src_data[3];
			}
			break;

		//---------------------------------------------------------------------
		case 3 :
		//---------------------------------------------------------------------
			for(int w=m_width;w;--w,p_src_data+=4)
			{
				*p_out_data++ = p_src_data[2];
				*p_out_data++ = p_src_data[1];
				*p_out_data++ = p_src_data[0];
			}
			break;

		//---------------------------------------------------------------------
		case 2 :
		//---------------------------------------------------------------------
			for(int w=m_width;w;--w,p_src_data+=4)
			{
				*p_out_data++ = ((p_src_data[0]*85) + (p_src_data[1]*85) + (p_src_data[2]*85))>>8;
				*p_out_data++ = p_src_data[3];
			}
			break;

		//---------------------------------------------------------------------
		case 1 :
		//---------------------------------------------------------------------
			for(int w=m_width;w;--w,p_src_data+=4)
				*p_out_data++ = ((p_src_data[0]*85) + (p_src_data[1]*85) + (p_src_data[2]*85))>>8;
			break;

		default : break;
	}
}

void
PNGDecode::process_scanline_truecolour(const std::uint8_t * p_src_data,std::uint8_t * p_out_data)
{
	if(m_depth != 8)
		return;

	switch(m_out_components)
	{
		//---------------------------------------------------------------------
		case 4 :
		//---------------------------------------------------------------------
			for(int w=m_width;w;--w,p_src_data+=3)
			{
				*p_out_data++ = p_src_data[2];
				*p_out_data++ = p_src_data[1];
				*p_out_data++ = p_src_data[0];
				*p_out_data++ = 0x0FF;
			}
			break;

		//---------------------------------------------------------------------
		case 3 :
		//---------------------------------------------------------------------
			//memcpy(p_out_data,p_src_data,m_width*3);
			for(int w=m_width;w;--w,p_src_data+=3)
			{
				*p_out_data++ = p_src_data[2];
				*p_out_data++ = p_src_data[1];
				*p_out_data++ = p_src_data[0];
			}
			break;

		//---------------------------------------------------------------------
		case 2 :
		//---------------------------------------------------------------------
			for(int w=m_width;w;--w,p_src_data+=3)
			{
				*p_out_data++ = ((p_src_data[0]*85) + (p_src_data[1]*85) + (p_src_data[2]*85))>>8;
				*p_out_data++ = 0x0FF;
			}
			break;

		//---------------------------------------------------------------------
		case 1 :
		//---------------------------------------------------------------------
			for(int w=m_width;w;--w,p_src_data+=3)
				*p_out_data++ = ((p_src_data[0]*85) + (p_src_data[1]*85) + (p_src_data[2]*85))>>8;
			break;

		default : break;
	}
}

void
PNGDecode::process_scanline_greyscale(const std::uint8_t * p_src,std::uint8_t * p_out)
{
	if(m_depth == 8)
	{
		switch(m_out_components)
		{
			case 4 :	for(int w=m_width;w;--w) {const auto d=*p_src++;*p_out++=d;*p_out++=d;*p_out++=d;*p_out++=0x0FF;} break;
			case 3 :	for(int w=m_width;w;--w) {const auto d=*p_src++;*p_out++=d;*p_out++=d;*p_out++=d;} break;
			case 2 :	for(int w=m_width;w;--w) {*p_out++=*p_src++;*p_out++=0x0FF;}	break;
			case 1 :	std::memcpy(p_out,p_src,m_width);	break;
			default : break;
		}
	}
	else
	{
		BitStreamIn bits(p_src,m_scanline_size-1,m_depth);

		switch(m_out_components)
		{
			case 4 :	for(int w=m_width;w;--w) {const auto d=bits.read_byte_repl();*p_out++=d;*p_out++=d;*p_out++=d;*p_out++=0x0FF;} break;
			case 3 :	for(int w=m_width;w;--w) {const auto d=bits.read_byte_repl();*p_out++=d;*p_out++=d;*p_out++=d;} break;
			case 2 :	for(int w=m_width;w;--w) {*p_out++=bits.read_byte_repl();*p_out++=0x0FF;} break;
			case 1 :	for(int w=m_width;w;--w) {*p_out++=bits.read_byte_repl();} break;
			default : break;
		}
	}
}

void
PNGDecode::process_scanline_greyscale_alpha(const std::uint8_t * p_src,std::uint8_t * p_out)
{
	if(m_depth == 8)
	{
		switch(m_out_components)
		{
			case 4 :	for(int w=m_width;w;--w) {const auto d=*p_src++;*p_out++=d;*p_out++=d;*p_out++=d;*p_out++=*p_src++;} break;
			case 3 :	for(int w=m_width;w;--w,p_src+=2) {const auto d=*p_src;*p_out++=d;*p_out++=d;*p_out++=d;} break;
			case 2 :	std::memcpy(p_out,p_src,m_width*2);	break;
			case 1 :	for(int w=m_width;w;--w,p_src+=2) {*p_out++=*p_src;}	break;
			default : break;
		}
	}
	else if(m_depth < 8)
	{
		BitStreamIn bits(p_src,m_scanline_size-1,m_depth);

		switch(m_out_components)
		{
			case 4 :	for(int w=m_width;w;--w) {const auto d=bits.read_byte_repl();*p_out++=d;*p_out++=d;*p_out++=d;*p_out++=bits.read_byte_repl();} break;
			case 3 :	for(int w=m_width;w;--w) {const auto d=bits.read_byte_repl();*p_out++=d;*p_out++=d;*p_out++=d;bits.read_byte_repl();} break;
			case 2 :	for(int w=m_width;w;--w) {*p_out++=bits.read_byte_repl();*p_out++=bits.read_byte_repl();} break;
			case 1 :	for(int w=m_width;w;--w) {*p_out++=bits.read_byte_repl();bits.read_byte_repl();} break;
			default : break;
		}
	}
}

void
PNGDecode::process_scanline_indexed(const std::uint8_t * p_src,std::uint8_t * p_out)
{
	if(m_depth == 8)
	{
		switch(m_out_components)
		{
			case 4 :	for(int w=m_width;w;--w,p_out+=4) {palette(*p_src++,p_out[2],p_out[1],p_out[0]);p_out[3]=0x0FF;} break;
			case 3 :	for(int w=m_width;w;--w,p_out+=3) {palette(*p_src++,p_out[2],p_out[1],p_out[0]);} break;
			case 2 :	for(int w=m_width;w;--w) {*p_out++=palette_grey(*p_src++);*p_out++=0x0FF;} break;
			case 1 :	for(int w=m_width;w;--w) {*p_out++=palette_grey(*p_src++);} break;
			default : break;
		}
	}
	else if(m_depth < 8)
	{
		BitStreamIn bits(p_src,m_scanline_size-1,m_depth);

		switch(m_out_components)
		{
			case 4 :	for(int w=m_width;w;--w,p_out+=4) {palette(bits.read(),p_out[2],p_out[1],p_out[0]);p_out[3]=0x0FF;} break;
			case 3 :	for(int w=m_width;w;--w,p_out+=3) {palette(bits.read(),p_out[2],p_out[1],p_out[0]);} break;
			case 2 :	for(int w=m_width;w;--w) {*p_out++=palette_grey(bits.read());*p_out++=0x0FF;} break;
			case 1 :	for(int w=m_width;w;--w) {*p_out++=palette_grey(bits.read());} break;
			default : break;
		}
	}
}


} // namespace adepng

