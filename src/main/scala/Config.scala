package cpu

import chisel3._
import chisel3.util._

object Config {
  def hasPerformanceCounter:  Boolean = true
  def hasDPIC:                Boolean = true
  def isSTA:                  Boolean = true
  def SoC:                    Boolean = true

  object ICacheConfig {
    def numOfCache:   Int = 32
    def sizeOfCache:  Int = 128 /* Bits */
    def ways:         Int = 4
    def burstLen:     Int = 4
    def burstSize:    Int = 16 /* Bytes */
    def m: 		        Int = log2Ceil(sizeOfCache >> 3)
	  def n:			      Int = log2Up(numOfCache/ways)
  }

  object BPConfig {
    def depthOfTable: Int = 8
    def offsetWidth:  Int = 2
    def tagWidth:     Int = 10
    def way:          Int = 1
  }
}
