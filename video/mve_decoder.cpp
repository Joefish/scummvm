/* ScummVM - Graphic Adventure Engine
 *
 * ScummVM is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/* Based on ffmpeg code (commit 039ebaa5f39ef45444f3cc42ab2ff71b0e9a1161)
 *
 * Interplay MVE File Demuxer
 * Copyright (c) 2003 The FFmpeg project
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "audio/audiostream.h"
#include "audio/mixer.h"
#include "audio/decoders/adpcm.h"
#include "audio/decoders/raw.h"
#include "common/endian.h"
#include "common/rect.h"
#include "common/stream.h"
#include "common/system.h"
#include "common/textconsole.h"
#include "graphics/surface.h"
#include "image/codecs/codec.h"
#include "video/mve_decoder.h"

namespace Video {

MveDecoder::MveDecoder()
    : _fileStream(nullptr)
    , _audioTrack(nullptr)
    , _videoTrack(nullptr)
    , _lastAddedTrack(nullptr)
    , _nextChunkOffset(0)
    , _screenUpdateRequested(false) {
}

MveDecoder::~MveDecoder() {
	close();
}

static const char signature[] = "Interplay MVE File\x1A\0\x1A";

bool MveDecoder::loadStream(Common::SeekableReadStream *stream) {
	close();

	this->_fileStream = stream;
	byte signature_buffer[sizeof(signature)];
	stream->read(signature_buffer, sizeof(signature_buffer));

	// NOTE: ffmpeg assumes there may be a data before the signature and keeps
	//       'shifting' bytes from the stream to the signare_buffer until the
	//       comparision is successful or the stream ends.
	//       Right now, the decoder is only used for Kingdom and its movie files
	//       don't have any additional data before the signature.
	while (memcmp(signature_buffer, signature, sizeof(signature))) {
		memmove(signature_buffer, signature_buffer + 1, sizeof(signature_buffer) - 1);
		signature_buffer[sizeof(signature_buffer) - 1] = stream->readByte();
		if (stream->eos()) {
			warning("MveDecoder::loadStream(): attempted to load non-MVE data");
			return false;
		}
	}

	// skip the remaining unused parameters ((x1A00) x0010 x3311)
	stream->skip(4);
	_nextChunkOffset = stream->pos();

	// read the first two chunk headers and check if they are valid
	uint16 chunkSize = stream->readUint16LE();
	ChunkType chunkType = (ChunkType)stream->readUint16LE();
	stream->skip(chunkSize);
	if (chunkType != ChunkTypeInitVideo)
		return false;

	chunkSize = stream->readUint16LE();
	chunkType = (ChunkType)stream->readUint16LE();
	stream->seek(_nextChunkOffset);
	if (chunkType != ChunkTypeInitAudio && chunkType != ChunkTypeVideo)
		return false;

	return true;
}

void MveDecoder::readNextPacket() {
	bool readNextChunk = true;
	byte scratchBuffer[1024] = {};

	while (readNextChunk) {
		uint16 chunkSize = _fileStream->readUint16LE();
		ChunkType chunkType = (ChunkType)_fileStream->readUint16LE();
		if (_fileStream->eos())
			chunkType = ChunkTypeEOF;

		switch (chunkType) {
		case ChunkTypeBad:
		case ChunkTypeEOF:
		case ChunkTypeNoMemory:
		case ChunkTypeDone:
			error("Encountered bad chunk type (error 0x%04X)", chunkType);
			break;
		case ChunkTypeEnd:
		case ChunkTypeShutdown:
		case ChunkTypeVideo:
			readNextChunk = false;
			break;
		case ChunkTypeAudioOnly:
			break;
		case ChunkTypeInitAudio:
			break;
		case ChunkTypeInitVideo:
			if (!_videoTrack)
				_videoTrack = new MveVideoTrack();
			break;
		default:
			warning("Encountered unknown chunk type (error 0x%04X)", chunkType);
			_fileStream->skip(chunkSize);
			chunkType = ChunkTypeBad;
			break;
		}

		// Opcode meaning does not depend on chunk type
		while (chunkSize > 0 && chunkType != ChunkTypeBad) {
			uint opcodeSize = _fileStream->readUint16LE();
			Opcode opcodeType = (Opcode)_fileStream->readByte();
			uint opcodeVersion = _fileStream->readByte();

			chunkSize -= kOpcodePreambleSize + opcodeSize;
			assert(chunkSize >= 0);

			switch (opcodeType) {
			case OpcodeCreateTimer:
				_videoTrack->op02CreateTimer(_fileStream, opcodeSize, opcodeType, opcodeVersion);
				break;
			case OpcodeInitAudioBuffers: {
				initAudio(_fileStream, opcodeSize, opcodeType, opcodeVersion);
			} break;
			case OpcodeInitVideoBuffers:
				_videoTrack->op05InitVideoBuffers(_fileStream, opcodeSize, opcodeType, opcodeVersion);
				break;
			case OpcodeVideoData06:
				_videoTrack->op06VideoData(_fileStream, opcodeSize, opcodeType, opcodeVersion);
				break;
			case OpcodeSendBuffer:
				_screenUpdateRequested = true;
				// NOTE: skipped opcode contains palette indices that should be updated
				//       according to docs. VGA legacy?
				_fileStream->skip(opcodeSize);
				break;
			case OpcodeAudioFrame:
				// TODO: ffmpeg just saves chunk offset and size for later processing
				//       But why not just add it to the queue/packet stream from here?
				// TODO: RIP audio for now until audio decoder is implemented
				//       (or just wing it in here for now to get a proof of concept
				//        and refactor it to audio ADPCM later)
				_audioTrack->op08AudioFrame(_fileStream, opcodeSize, opcodeType, opcodeVersion);
				break;
			case OpcodeSetPalette:
				_videoTrack->op0CSetPalette(_fileStream, opcodeSize, opcodeType, opcodeVersion);
				break;
			case OpcodeSetSkipMap:
				_videoTrack->op0ESetSkipMap(_fileStream, opcodeSize, opcodeType, opcodeVersion);
				break;
			case OpcodeSetDecodingMap:
				_videoTrack->op0FSetDecodingMap(_fileStream, opcodeSize, opcodeType, opcodeVersion);
				break;
			case OpcodeVideoData10:
				_videoTrack->op10VideoData(_fileStream, opcodeSize, opcodeType, opcodeVersion);
				break;
			case OpcodeVideoData11:
				_videoTrack->op11VideoData(_fileStream, opcodeSize, opcodeType, opcodeVersion);
				break;
			case OpcodeStartStopAudio:
			case OpcodeEndOfChunk:
			case OpcodeEndOfStream:
			case OpcodeSilenceFrame:
			case OpcodeInitVideoMode:
			case OpcodeSetPaletteCompressed:
			case OpcodeCreateGradient:
			case OpcodeUnknown12:
			case OpcodeUnknown13:
			case OpcodeUnknown14:
			case OpcodeUnknown15:
				_fileStream->skip(opcodeSize);
				break;
			default:
				error("unknown opcode type!");
				break;
			}
		}
	}

	// TODO: remove comment
	// if chunk type == VIDEO || AUDIO then send packet to decoder
}

bool MveDecoder::initAudio(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion) {
	byte scratchBuffer[1024];

	if (opcodeVersion > 1 || opcodeSize > 10 || opcodeSize < 6) {
		warning("bad InitAudioBuffers Opcode!");
		return false;
	}
	if (stream->read(scratchBuffer, opcodeSize) != opcodeSize) {
		warning("Read error!");
		return false;;
	}

	uint16 audioSampleRate = READ_LE_UINT16(&scratchBuffer[4]);
	uint16 audioFlags = READ_LE_UINT16(&scratchBuffer[2]);
	// bit 0: 0 == mono, 1 == stereo
	// bit 1: 0 == 8bit, 1 == 16bit
	// bit 2: compression on/off if opcodeVersion == 1
	uint audioChannels = (audioFlags & 0x01) + 1;
	uint audioBits = (((audioFlags & 0x02) >> 1) + 1) * 8;
	AudioType audioType;
	if ((opcodeVersion == 1) && (audioFlags & 0x04)) {
		audioType = AudioTypeDPCM;
		// TODO: half bitrate? => (channels * sampleRate * bitsPerCodedSample) / 2)
	} else if (audioBits == 16) {
		audioType = AudioTypePCMS16LE;
	} else {
		audioType = AudioTypePCMU8;
	}

	if (!_audioTrack)
		_audioTrack = new MveAudioTrack(Audio::Mixer::kPlainSoundType,
		                                audioType, audioBits, audioChannels, audioSampleRate);

	return true;
}


// -- MveAudioTrack -----------------------------------------------------------

MveDecoder::MveAudioTrack::MveAudioTrack(Audio::Mixer::SoundType soundType, AudioType mveAudioType,
        int bits, int channels, int sampleRate)
    : AudioTrack(soundType)
    , _audioStream(nullptr)
    , _audioChunkOffset(0)
    , _audioChunkSize(0)
    , _audioBits(bits)
    , _audioChannels(channels)
    , _audioSampleRate(sampleRate)
    , _audioFrameCount(0)
    , _audioType(mveAudioType)
    , _audioStreamIndex(0) {
}

// TODO:
//      Interplay DPCM is NOT equivalent to microsoft ADPCM or any other implemented DPCM format.....
//      Fortunately the format is not complex but extending the scummvm lib takes more care and
//      reading and API conventions and ..
void MveDecoder::MveAudioTrack::createAudioStream() {
	_audioStream = nullptr;
	switch (_audioType) {
	case AudioTypeDPCM:
		_audioStream = Audio::makePacketizedADPCMStream(Audio::kADPCMMS, _audioSampleRate, _audioChannels);
		break;
	case AudioTypePCMU8:
		_audioStream = Audio::makePacketizedRawStream(_audioSampleRate, (byte)_audioChannels);
		break;
	case AudioTypePCMS16LE:
	case AudioTypeNone:
	default:
		warning("Cannot create audio stream. Unhandled type!");
		// TODO: Does this work? wouldn't queuepacket() be unimplemented?
		_audioStream = dynamic_cast<Audio::PacketizedAudioStream*>(Audio::makeNullAudioStream());
		break;
	}
}

void MveDecoder::MveAudioTrack::queueSound(Common::SeekableReadStream *stream) {
	if (_audioStream)
		_audioStream->queuePacket(stream);
	else
		delete stream;
}

Audio::AudioStream *MveDecoder::MveAudioTrack::getAudioStream() const {
	return _audioStream;
}


bool MveDecoder::MveAudioTrack::op08AudioFrame(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion) {
	// TODO: skip for now


	// prep data
	/*
	if (s->audio_chunk_offset && s->audio_channels && s->audio_bits) {
		if (s->audio_type == AV_CODEC_ID_NONE) {
			av_log(s->avf, AV_LOG_ERROR, "Can not read audio packet before"
				   "audio codec is known\n");
				return CHUNK_BAD;
		}

		//adjust for PCM audio by skipping chunk header
		if (s->audio_type != AV_CODEC_ID_INTERPLAY_DPCM) {
			s->audio_chunk_offset += 6;
			s->audio_chunk_size -= 6;
		}

		avio_seek(pb, s->audio_chunk_offset, SEEK_SET);
		s->audio_chunk_offset = 0;

		if (s->audio_chunk_size != av_get_packet(pb, pkt, s->audio_chunk_size))
			return CHUNK_EOF;

		pkt->stream_index = s->audio_stream_index;
		pkt->pts = s->audio_frame_count;

		// audio frame maintenance
		if (s->audio_type != AV_CODEC_ID_INTERPLAY_DPCM)
			s->audio_frame_count +=
			(s->audio_chunk_size / s->audio_channels / (s->audio_bits / 8));
		else
			s->audio_frame_count +=
				(s->audio_chunk_size - 6 - s->audio_channels) / s->audio_channels;

		av_log(s->avf, AV_LOG_TRACE, "sending audio frame with pts %"PRId64" (%d audio frames)\n",
				pkt->pts, s->audio_frame_count);

		chunk_type = CHUNK_VIDEO;
	 */


	stream->skip(opcodeSize);
	return false;
}


// -- MveVideoTrack -----------------------------------------------------------

MveDecoder::MveVideoTrack::MveVideoTrack()
	: _videoPresentationTimestamp(0)
	, _palette()
	, _videoBpp(0)
	, _videoWidth(0)
	, _videoHeight(0)
	, _videoFrameFormat(0)
	, _videoStreamIndex(0)
	, _dirtyPalette(0)
	, _hasPalette(0)
	, _videoChunk(nullptr)
	, _skipMap(nullptr)
	, _decodeMap(nullptr)
	, _motionVector(nullptr) {
}

uint16 MveDecoder::MveVideoTrack::getWidth() const {
	return _videoWidth;
}

uint16 MveDecoder::MveVideoTrack::getHeight() const {
	return _videoHeight;
}

Graphics::PixelFormat MveDecoder::MveVideoTrack::getPixelFormat() const {
	return _currentFrame->format;
}

int MveDecoder::MveVideoTrack::getCurFrame() const {
	return _videoStreamIndex;
}

int MveDecoder::MveVideoTrack::getFrameCount() const {
	return _videoFrameCount;
}

Common::Rational MveDecoder::MveVideoTrack::getFrameRate() const {
	return _videoFrameRate;
}

void MveDecoder::MveVideoTrack::setHasDirtyPalette() {
	_dirtyPalette = true;
}

bool MveDecoder::MveVideoTrack::hasDirtyPalette() const {
	return _dirtyPalette;
}

const byte *MveDecoder::MveVideoTrack::getPalette() const {
	_dirtyPalette = false;
	return _palette;
}

const Graphics::Surface *MveDecoder::MveVideoTrack::decodeNextFrame() {

}

bool MveDecoder::MveVideoTrack::initVideo(uint width, uint height, uint bpp) {
	// TODO: Apparently video resolution and bit depth could differ in the same file.
	//       Afaik Kingdom does not but for broader compatibility should be implemented.

	_videoWidth = width;
	_videoHeight = height;
	_videoBpp = bpp;

	// TODO: alloc last/second last/current decoded/prev decoded frame
	//       if bpp == 16 then RGB555 else PAL8
}

bool MveDecoder::MveVideoTrack::op05InitVideoBuffers(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion) {
	byte scratchBuffer[1024];

	if ((opcodeVersion > 2) || (opcodeSize > 8) || opcodeSize < 4 || (opcodeVersion == 2 && opcodeSize < 8)) {
		warning("bad InitVideoBuffer Opcode!");
		return false;
	}
	if (stream->read(scratchBuffer, opcodeSize) != opcodeSize) {
		warning("Read error!");
		return false;
	}

	uint width = READ_LE_UINT16(&scratchBuffer[0]);
	uint height = READ_LE_UINT16(&scratchBuffer[2]);
	uint bpp = 16;
	if (opcodeVersion < 2 || !READ_LE_UINT16(&scratchBuffer[6]))
		bpp = 8;

	initVideo(width, height, bpp);
	return true;
}


bool MveDecoder::MveVideoTrack::op0CSetPalette(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion) {
	byte scratchBuffer[1024];

	if (opcodeSize > 0x304 || opcodeSize < 4) {
		warning("SetPalette Opcode: Invalid palette size");
		return false;
	}
	if (stream->read(scratchBuffer, opcodeSize) != opcodeSize) {
		warning("Read error!");
		return false;
	}

	uint16 firstColor = READ_LE_UINT16(&scratchBuffer[0]);
	uint16 lastColor = READ_LE_UINT16(&scratchBuffer[2]) - 1;
	if ((firstColor > 0xFF) || (lastColor > 0xFF) ||
	    (lastColor - firstColor + 1) * 3 + 4 > opcodeSize) {
		warning("Opcode SetPalette: indices out of range (%d -> %d)", firstColor, lastColor);
		return false;
	}

	for (uint16 i = firstColor, j = 0; i <= lastColor;) {
		// palette stored as 6bit VGA so we shift them up to 8bit range
		byte r = scratchBuffer[j++] << 2;
		byte g = scratchBuffer[j++] << 2;
		byte b = scratchBuffer[j++] << 2;
		uint32 pixelColor = (r << 16) | (g << 8) | (b);
		pixelColor |= (pixelColor >> 6) & 0x030303; // set low 2-bit to hi 2-bit
		_palette[i++] = pixelColor & 0xFF0000;
		_palette[i++] = pixelColor & 0x00FF00;
		_palette[i++] = pixelColor & 0x0000FF;
	}

	_dirtyPalette = true;

	return true;
}

// TODO: either log and skip and decode some time later or just handle them
bool MveDecoder::MveVideoTrack::op0ESetSkipMap(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion) {
	int32 offset = stream->pos();
	uint size = opcodeSize;
	uint32 readBytes = stream->read(_skipMapBuffer, size);
	assert(readBytes == size);

	delete _skipMap;
	_skipMap = new Common::MemoryReadStream(_skipMapBuffer, size);

	return true;
}

bool MveDecoder::MveVideoTrack::op0FSetDecodingMap(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion) {
	int32 offset = stream->pos();
	uint size = opcodeSize;
	uint32 readBytes = stream->read(_decodeMapBuffer, size);
	assert(readBytes == size);

	delete _decodeMap;
	_decodeMap = new Common::MemoryReadStream(_decodeMapBuffer, size);

	return true;
}

bool MveDecoder::MveVideoTrack::op06VideoData(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion) {
	// NOTE: logged in ipmovie
	_videoFrameFormat = 0x06;

	return true;
}

bool MveDecoder::MveVideoTrack::op10VideoData(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion) {
	// NOTE: logged in ipmovie
	_videoFrameFormat = 0x10;

	return true;
}

bool MveDecoder::MveVideoTrack::op11VideoData(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion) {
	// NOTE: logged in ipmovie
	_videoFrameFormat = 0x11;

	// set motion vector index to videoChunk + 14, if bpp16
	_motionVectorOffset = _videoChunkOffset + 14; // 14 bytes opcode data
	_motionVectorOffset += READ_LE_UINT16(&_videoChunk[_videoChunkOffset]);
	_videoChunkOffset += 2;

	return true;
}

// -- MveVideoTrack -- Decode Functions

bool MveDecoder::MveVideoTrack::copyFrom(const Graphics::Surface *src, Graphics::Surface *dest, int dx, int dy) {
	if (src == nullptr || dest == nullptr)
		return false;

	const int isVideo16Bit = _videoBpp == 16 ? 1 : 0;
	int width = dest->w;
	int currentOffset = _currentFramePixelOffset;
	// NOTE: is dest->pitch the scanline size we need for decoding?
	//       its just for offset calc so yes, it's fine
	int x = (currentOffset % dest->pitch) / (1 + isVideo16Bit);
	int y = currentOffset / dest->pitch;
	int deltaX = dx + x - ((dx + x >= width) - (dx + x < 0)) * width;
	int deltaY = dy + y - ((dx + x >= width) - (dx + x < 0));
	int motionOffset = deltaY * src->pitch + deltaX * (1 + isVideo16Bit);

	if (motionOffset < 0) {
		return false; // invalid data
	} else if (motionOffset > _upperMotionLimitOffset) {
		return false; // invalid data
	}

	// TODO: is pixel_ptr actually always equ to dest?
	// TODO: If there's a problem Im sure its here..
	int blockWidth = isVideo16Bit ? 16 : 8;
	for (int blockY = 0; blockY < 8; blockY++) {
		for (int blockX = 0; blockX < blockWidth; blockX++) {
			byte *destPtr = (byte *)dest->getPixels() + (y + blockY) * dest->pitch +
			                (x + blockX) * dest->format.bytesPerPixel;
			byte *sourcePtr = (byte *)src->getPixels() + (y + blockY) * src->pitch +
			                  (x + blockX) * src->format.bytesPerPixel;
			*destPtr = *sourcePtr;
		}
	}

	return true;
}

int MveDecoder::MveVideoTrack::decodeBlockOpcode0x00(Graphics::Surface *frame) {
	return copyFrom(_lastFrame, frame, 0, 0);
}

int MveDecoder::MveVideoTrack::decodeBlockOpcode0x01(Graphics::Surface *frame) {
	return copyFrom(_secondlastFrame, frame, 0, 0);
}

int MveDecoder::MveVideoTrack::decodeBlockOpcode0x02(Graphics::Surface *frame) {
	int motionByte = 0;
	int x = 0;
	int y = 0;

	if (_videoBpp != 16) {
		motionByte = _videoChunk[_videoChunkOffset++];
	} else {
		motionByte = _videoChunk[_motionVectorOffset++];
	}

	if (motionByte < 56) {
		x = 8 + (motionByte % 7);
		y = motionByte / 7;
	} else {
		x = -14 + ((motionByte - 56) % 29);
		y = 8 + ((motionByte - 56) / 29);
	}

	return copyFrom(_secondlastFrame, frame, x, y);
}

int MveDecoder::MveVideoTrack::decodeBlockOpcode0x03(Graphics::Surface *frame) {
	int motionByte = 0;
	int x = 0;
	int y = 0;

	if (_videoBpp != 16) {
		motionByte = _videoChunk[_videoChunkOffset++];
	} else {
		motionByte = _videoChunk[_motionVectorOffset++];
	}

	if (motionByte < 56) {
		x = -(8 + (motionByte % 7));
		y = -(motionByte / 7);
	} else {
		x = -(-14 + ((motionByte - 56) % 29));
		y = -(8 + ((motionByte - 56) / 29));
	}

	return copyFrom(frame, frame, x, y);
}

int MveDecoder::MveVideoTrack::decodeBlockOpcode0x04(Graphics::Surface *frame){
	int x = 0;
	int y = 0;
	int motionByte = 0;
	int motionByteLow = 0;
	int motionByteHigh = 0;

	/* copy a block from the previous frame; need 1 more byte */
	if (_videoBpp != 16) {
		motionByte = _videoChunk[_videoChunkOffset++];
	} else {
		motionByte = _videoChunk[_motionVectorOffset++];
	}

	motionByteLow = motionByte & 0x0F;
	motionByteHigh = (motionByte >> 4) & 0x0F;
	x = -8 + motionByteLow;
	y = -8 + motionByteHigh;

	return copyFrom(_lastFrame, frame, x, y);
}

int MveDecoder::MveVideoTrack::decodeBlockOpcode0x05(Graphics::Surface *frame){
	int x = _videoChunk[_videoChunkOffset++];
	int y = _videoChunk[_videoChunkOffset++];

	return copyFrom(_lastFrame, frame, x, y);
}

int MveDecoder::MveVideoTrack::decodeBlockOpcode0x06(Graphics::Surface *frame){
	// mystery opcode

	return 0;
}

int MveDecoder::MveVideoTrack::decodeBlockOpcode0x07(Graphics::Surface *frame){
	int x = 0;
	int y = 0;
	byte p[2] = {0, 0};
	uint flags = 0;

	if (_videoChunk)
}

int MveDecoder::MveVideoTrack::decodeBlockOpcode0x08(Graphics::Surface *frame){

}

int MveDecoder::MveVideoTrack::decodeBlockOpcode0x09(Graphics::Surface *frame){

}

int MveDecoder::MveVideoTrack::decodeBlockOpcode0x0A(Graphics::Surface *frame){

}

int MveDecoder::MveVideoTrack::decodeBlockOpcode0x0B(Graphics::Surface *frame){

}

int MveDecoder::MveVideoTrack::decodeBlockOpcode0x0C(Graphics::Surface *frame){

}

int MveDecoder::MveVideoTrack::decodeBlockOpcode0x0D(Graphics::Surface *frame){

}

int MveDecoder::MveVideoTrack::decodeBlockOpcode0x0E(Graphics::Surface *frame){

}

int MveDecoder::MveVideoTrack::decodeBlockOpcode0x0F(Graphics::Surface *frame){

}

int MveDecoder::MveVideoTrack::decodeBlock16Opcode0x06(Graphics::Surface *frame){

}

int MveDecoder::MveVideoTrack::decodeBlock16Opcode0x07(Graphics::Surface *frame){

}

int MveDecoder::MveVideoTrack::decodeBlock16Opcode0x08(Graphics::Surface *frame){

}

int MveDecoder::MveVideoTrack::decodeBlock16Opcode0x09(Graphics::Surface *frame){

}

int MveDecoder::MveVideoTrack::decodeBlock16Opcode0x0A(Graphics::Surface *frame){

}

int MveDecoder::MveVideoTrack::decodeBlock16Opcode0x0B(Graphics::Surface *frame){

}

int MveDecoder::MveVideoTrack::decodeBlock16Opcode0x0C(Graphics::Surface *frame){

}

int MveDecoder::MveVideoTrack::decodeBlock16Opcode0x0D(Graphics::Surface *frame){

}

int MveDecoder::MveVideoTrack::decodeBlock16Opcode0x0E(Graphics::Surface *frame){

}

} // End of namespace Video
