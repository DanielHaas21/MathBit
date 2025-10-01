import LogoSvg from '../assets/images/insio-logo.svg?react';
import { cn } from '../utils';

export interface LogoProps {
  className?: string;
  onClick?: React.MouseEventHandler<SVGSVGElement> | undefined;
}

export const Logo = (props: LogoProps) => {
  const { className, onClick } = props;
  return <LogoSvg className={cn(className)} onClick={onClick} />;
};
