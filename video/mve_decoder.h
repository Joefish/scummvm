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

#ifndef VIDEO_MVEDECODER_H
#define VIDEO_MVEDECODER_H

#include "audio/mixer.h"
#include "video/video_decoder.h"
#include "common/list.h"
#include "common/memstream.h"
#include "common/rect.h"

namespace Audio {
class AudioStream;
class PacketizedAudioStream;
}

namespace Common {
class SeekableReadStream;
}

namespace Image {
class Codec;
}

namespace Graphics {
struct PixelFormat;
struct Surface;
}

namespace Video {

/**
 * Decoder for Interplay MVE videos.
 *
 * Video decoder used in engines:
 *  - kingdom
 */
class MveDecoder : public VideoDecoder {
public:
	MveDecoder();
	virtual ~MveDecoder() override;
	virtual bool loadStream(Common::SeekableReadStream *stream) override;
	virtual void readNextPacket() override;

protected:
	static const int kChunkPreambleSize = 4;
	static const int kOpcodePreambleSize = 4;
	static const int kPaletteSize = 1024;

	enum ChunkType {
		ChunkTypeInitAudio = 0x0000,
		ChunkTypeAudioOnly = 0x0001,
		ChunkTypeInitVideo = 0x0002,
		ChunkTypeVideo     = 0x0003,
		ChunkTypeShutdown  = 0x0004,
		ChunkTypeEnd       = 0x0005,
		// only used internally
		ChunkTypeDone      = 0xFFFC,
		ChunkTypeNoMemory  = 0xFFFD,
		ChunkTypeEOF       = 0xFFFE,
		ChunkTypeBad       = 0xFFFF
	};

	enum Opcode {
		OpcodeEndOfStream           = 0x00,
		OpcodeEndOfChunk            = 0x01,
		OpcodeCreateTimer           = 0x02,
		OpcodeInitAudioBuffers      = 0x03,
		OpcodeStartStopAudio        = 0x04,
		OpcodeInitVideoBuffers      = 0x05,
		OpcodeVideoData06           = 0x06,
		OpcodeSendBuffer            = 0x07,
		OpcodeAudioFrame            = 0x08,
		OpcodeSilenceFrame          = 0x09,
		OpcodeInitVideoMode         = 0x0A,
		OpcodeCreateGradient        = 0x0B,
		OpcodeSetPalette            = 0x0C,
		OpcodeSetPaletteCompressed  = 0x0D,
		OpcodeSetSkipMap            = 0x0E,
		OpcodeSetDecodingMap        = 0x0F,
		OpcodeVideoData10           = 0x10,
		OpcodeVideoData11           = 0x11,
		OpcodeUnknown12             = 0x12,
		OpcodeUnknown13             = 0x13,
		OpcodeUnknown14             = 0x14,
		OpcodeUnknown15             = 0x15
	};

	enum AudioType {
		AudioTypeNone,
		AudioTypeDPCM,
		AudioTypePCMS16LE,
		AudioTypePCMU8
	};

	class MveVideoTrack : public FixedRateVideoTrack {
	public:
		MveVideoTrack();
		~MveVideoTrack();

		virtual const Graphics::Surface *decodeNextFrame() override;
		virtual uint16 getWidth() const override;
		virtual uint16 getHeight() const override;
		virtual int getCurFrame() const override;
		virtual int getFrameCount() const override;
		virtual Graphics::PixelFormat getPixelFormat() const override;
		virtual const byte *getPalette() const override;
		virtual bool hasDirtyPalette() const override;
		void setHasDirtyPalette();

		// TODO: opcode handling
		bool op02CreateTimer(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion);
		bool op05InitVideoBuffers(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion);
		bool op06VideoData(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion);
		bool op0CSetPalette(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion);
		bool op0ESetSkipMap(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion);
		bool op0FSetDecodingMap(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion);
		bool op10VideoData(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion);
		bool op11VideoData(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion);
	protected:
		virtual Common::Rational getFrameRate() const override;

	private:
		using DecodeBlockFn = int (MveVideoTrack::*)(Graphics::Surface*);
		using DecodeBlock16Fn = int (MveVideoTrack::*)(Graphics::Surface*);

		bool initVideo(uint width, uint height, uint bpp);
		// opcode internals for 06/10/11Video
		bool copyFrom(const Graphics::Surface *src, Graphics::Surface *dest, int dx, int dy);
		int decodeBlockOpcode0x00(Graphics::Surface *frame);
		int decodeBlockOpcode0x01(Graphics::Surface *frame);
		int decodeBlockOpcode0x02(Graphics::Surface *frame);
		int decodeBlockOpcode0x03(Graphics::Surface *frame);
		int decodeBlockOpcode0x04(Graphics::Surface *frame);
		int decodeBlockOpcode0x05(Graphics::Surface *frame);
		int decodeBlockOpcode0x06(Graphics::Surface *frame);
		int decodeBlockOpcode0x07(Graphics::Surface *frame);
		int decodeBlockOpcode0x08(Graphics::Surface *frame);
		int decodeBlockOpcode0x09(Graphics::Surface *frame);
		int decodeBlockOpcode0x0A(Graphics::Surface *frame);
		int decodeBlockOpcode0x0B(Graphics::Surface *frame);
		int decodeBlockOpcode0x0C(Graphics::Surface *frame);
		int decodeBlockOpcode0x0D(Graphics::Surface *frame);
		int decodeBlockOpcode0x0E(Graphics::Surface *frame);
		int decodeBlockOpcode0x0F(Graphics::Surface *frame);
		int decodeBlock16Opcode0x06(Graphics::Surface *frame);
		int decodeBlock16Opcode0x07(Graphics::Surface *frame);
		int decodeBlock16Opcode0x08(Graphics::Surface *frame);
		int decodeBlock16Opcode0x09(Graphics::Surface *frame);
		int decodeBlock16Opcode0x0A(Graphics::Surface *frame);
		int decodeBlock16Opcode0x0B(Graphics::Surface *frame);
		int decodeBlock16Opcode0x0C(Graphics::Surface *frame);
		int decodeBlock16Opcode0x0D(Graphics::Surface *frame);
		int decodeBlock16Opcode0x0E(Graphics::Surface *frame);

		static constexpr DecodeBlockFn _decodeBlock[] = {
			&MveVideoTrack::decodeBlockOpcode0x00,
			&MveVideoTrack::decodeBlockOpcode0x01,
			&MveVideoTrack::decodeBlockOpcode0x02,
			&MveVideoTrack::decodeBlockOpcode0x03,
			&MveVideoTrack::decodeBlockOpcode0x04,
			&MveVideoTrack::decodeBlockOpcode0x05,
			&MveVideoTrack::decodeBlockOpcode0x06,
			&MveVideoTrack::decodeBlockOpcode0x07,
			&MveVideoTrack::decodeBlockOpcode0x08,
			&MveVideoTrack::decodeBlockOpcode0x09,
			&MveVideoTrack::decodeBlockOpcode0x0A,
			&MveVideoTrack::decodeBlockOpcode0x0B,
			&MveVideoTrack::decodeBlockOpcode0x0C,
			&MveVideoTrack::decodeBlockOpcode0x0D,
			&MveVideoTrack::decodeBlockOpcode0x0E,
			&MveVideoTrack::decodeBlockOpcode0x0F,
		};

		static constexpr DecodeBlockFn _decodeBlock16[] = {
			&MveVideoTrack::decodeBlockOpcode0x00,
			&MveVideoTrack::decodeBlockOpcode0x01,
			&MveVideoTrack::decodeBlockOpcode0x02,
			&MveVideoTrack::decodeBlockOpcode0x03,
			&MveVideoTrack::decodeBlockOpcode0x04,
			&MveVideoTrack::decodeBlockOpcode0x05,
			&MveVideoTrack::decodeBlock16Opcode0x06,
			&MveVideoTrack::decodeBlock16Opcode0x07,
			&MveVideoTrack::decodeBlock16Opcode0x08,
			&MveVideoTrack::decodeBlock16Opcode0x09,
			&MveVideoTrack::decodeBlock16Opcode0x0A,
			&MveVideoTrack::decodeBlock16Opcode0x0B,
			&MveVideoTrack::decodeBlock16Opcode0x0C,
			&MveVideoTrack::decodeBlock16Opcode0x0D,
			&MveVideoTrack::decodeBlock16Opcode0x0E,
			&MveVideoTrack::decodeBlockOpcode0x01,
		};

		Graphics::Surface *_currentFrame;
		Graphics::Surface *_lastFrame;
		Graphics::Surface *_secondlastFrame;

		static constexpr int kBufferSize = 0x10000;
		byte _videoChunkBuffer[kBufferSize];
		byte _skipMapBuffer[kBufferSize];
		byte _decodeMapBuffer[kBufferSize];
		// TODO: would be nice if you could reset MemoryStream
		Common::MemoryReadStream *_videoChunk;
		Common::MemoryReadStream *_skipMap;
		Common::MemoryReadStream *_decodeMap;
		Common::MemoryReadStream *_motionVector;     // used exclusively for videoOp 0x11

		int64 _videoPresentationTimestamp;
		byte _palette[3 * 256];
		int _videoBpp;
		int _videoWidth;
		int _videoHeight;
		int _videoFrameFormat;
		int _videoFrameCount;
		int _currentFramePixelOffset;
		int _upperMotionLimitOffset;
		Common::Rational _videoFrameRate;
		int _videoStreamIndex;
		// ?: was `int changed` before. does it actually stand for palette change?
		// !  changed gets set if frame properties change, like dimensions/bpp/..

		// TODO: it's an eyesore. please fix before push
		mutable int _dirtyPalette;
		bool _hasPalette; // isnt this just true if _videoBpp == 8?
		// byte _sendBuffer; // ?
	};

	class MveAudioTrack : public AudioTrack {
	public:
		MveAudioTrack(Audio::Mixer::SoundType soundType, MveDecoder::AudioType mveAudioType,
		        int bits, int channels, int sampleRate);
		~MveAudioTrack();

		virtual void createAudioStream();
		void queueSound(Common::SeekableReadStream *stream);

		bool op08AudioFrame(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion);
		// TODO: opcode handling
	protected:
		virtual Audio::AudioStream *getAudioStream() const;

	private:
		Audio::PacketizedAudioStream *_audioStream;

		// TODO: Is all this not stored in PacketizedAudioStream on creation?
		int64 _audioChunkOffset;
		int _audioChunkSize;
		int _audioBits;
		int _audioChannels;
		int _audioSampleRate;
		int _audioFrameCount;
		// TODO: Add enum (if not already availabe (pcm_u8, ..)
		AudioType _audioType;
		int _audioStreamIndex;
	};

private:
	bool initAudio(Common::SeekableReadStream *stream, uint opcodeSize, Opcode opcodeType, uint opcodeVersion);
	Common::SeekableReadStream *_fileStream;
	MveAudioTrack *_audioTrack;
	MveVideoTrack *_videoTrack;
	Track *_lastAddedTrack;
	int64 _nextChunkOffset;
	bool _screenUpdateRequested;
};

} // End of namespace Video

#endif
