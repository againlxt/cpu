package cpu

import chisel3._
import chisel3.util._

object Config {
  def hasPerformanceCounter:  Boolean = true
  def hasDPIC:                Boolean = true
  def isSTA:                  Boolean = false
  def SoC:                    Boolean = true

  object ICacheConfig {
    def numOfCache:   Int = 16
    def sizeOfCache:  Int = 128 /* Bits */
    def ways:         Int = 2
    def burstLen:     Int = 4
    def burstSize:    Int = 16 /* Bytes */
    def m: 		        Int = log2Ceil(sizeOfCache >> 3)
	  def n:			      Int = log2Up(numOfCache/ways)
  }
}
