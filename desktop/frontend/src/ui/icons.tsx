const icon = (path: string, solid = false) => (
  <svg className={solid ? "icon solid" : "icon"} viewBox="0 0 16 16" aria-hidden="true">
    <path d={path} />
  </svg>
);

export const PlayIcon = () => icon("M4 2.5v11a.6.6 0 0 0 .9.5l8.6-5.5a.6.6 0 0 0 0-1L4.9 2a.6.6 0 0 0-.9.5z", true);
export const PauseIcon = () => icon("M3.5 2.5h3v11h-3zM9.5 2.5h3v11h-3z", true);
export const StopIcon = () => icon("M4 3h8a1 1 0 0 1 1 1v8a1 1 0 0 1-1 1H4a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1z", true);
export const LoopIcon = () => icon("M3 9V7a3 3 0 0 1 3-3h7M11 2l2 2-2 2M13 7v2a3 3 0 0 1-3 3H3M5 14l-2-2 2-2");
export const VolumeIcon = () => icon("M2.5 6h2.5l3.5-3v10L5 10H2.5zM11 5.5a3.5 3.5 0 0 1 0 5M13 3.5a6.5 6.5 0 0 1 0 9");
