import { useState } from 'react';
import { Icon } from './Icon';
import { cn } from '../utils';

export interface copyToClipBoard {
  text: string;
}

export const ClipBoardCopy: React.FC<copyToClipBoard> = ({ text }) => {
  const [copied, setCopied] = useState<boolean>(false);

  return (
    <Icon
      name="copy"
      className={cn(' w-8 h-8 cursor-pointer transition', copied ? 'text-black-400' : 'text-black-800')}
      onClick={() => {
        navigator.clipboard.writeText(text);
        setCopied(true);
        setTimeout(() => {
          setCopied(false);
        }, 500);
      }}
    ></Icon>
  );
};

ClipBoardCopy.displayName = 'CopyToClipBoard';
