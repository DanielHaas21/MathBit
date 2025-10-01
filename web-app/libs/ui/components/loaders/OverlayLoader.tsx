'use client';
import React from 'react';
import { AnimatePresence, motion } from 'framer-motion';
import { Loader } from './Loader';

export interface OverlayLoaderProps {
  animation?: 'spin' | 'progressPulse' | null | undefined;
}

export const OverlayLoader: React.FC<OverlayLoaderProps> = ({ animation = 'spin' }) => {
  return (
    <AnimatePresence>
      <motion.div
        className="z-[100]"
        initial={{ opacity: 0 }}
        animate={{ opacity: 1 }}
        transition={{ duration: 0.3 }}
      >
        <div className="w-full h-[100vh] absolute top-[0px] left-[0px] flex items-center justify-center bg-overlays-50 ">
          <Loader size={200} strokeWidth={8} animation={animation} />
        </div>
      </motion.div>
    </AnimatePresence>
  );
};
