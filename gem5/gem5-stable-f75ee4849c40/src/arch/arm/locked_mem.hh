/*
 * Copyright (c) 2006 The Regents of The University of Michigan
 * Copyright (c) 2007-2008 The Florida State University
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met: redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer;
 * redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution;
 * neither the name of the copyright holders nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Authors: Ali Saidi
 *          Steve Reinhardt
 *          Stephen Hines
 */

#ifndef __ARCH_ARM_LOCKED_MEM_HH__
#define __ARCH_ARM_LOCKED_MEM_HH__

/**
 * @file
 *
 * ISA-specific helper functions for locked memory accesses.
 */

#include "arch/arm/miscregs.hh"
#include "mem/request.hh"

namespace ArmISA
{
template <class XC>
inline void
handleLockedRead(XC *xc, Request *req)
{
    xc->setMiscReg(MISCREG_LOCKADDR, req->getPaddr() & ~0xf);
    xc->setMiscReg(MISCREG_LOCKFLAG, true);
}


template <class XC>
inline bool
handleLockedWrite(XC *xc, Request *req)
{
    if (req->isSwap())
        return true;

    // Verify that the lock flag is still set and the address
    // is correct
    bool lock_flag = xc->readMiscReg(MISCREG_LOCKFLAG);
    Addr lock_addr = xc->readMiscReg(MISCREG_LOCKADDR);
    if (!lock_flag || (req->getPaddr() & ~0xf) != lock_addr) {
        // Lock flag not set or addr mismatch in CPU;
        // don't even bother sending to memory system
        req->setExtraData(0);
        xc->setMiscReg(MISCREG_LOCKFLAG, false);
        // the rest of this code is not architectural;
        // it's just a debugging aid to help detect
        // livelock by warning on long sequences of failed
        // store conditionals
        int stCondFailures = xc->readStCondFailures();
        stCondFailures++;
        xc->setStCondFailures(stCondFailures);
        if (stCondFailures % 100000 == 0) {
            warn("context %d: %d consecutive "
                 "store conditional failures\n",
                 xc->contextId(), stCondFailures);
        }

        // store conditional failed already, so don't issue it to mem
        return false;
    }
    return true;
}


} // namespace ArmISA

#endif
